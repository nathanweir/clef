(defpackage :clef-runner/src/project
  (:use :cl)
  (:import-from :clef-runner/src/runtime
                #:+exit-success+
                #:+exit-diagnostics+
                ;; Internal to the component; see compile.lisp.
                #:diagnostic-stream)
  (:import-from :clef-runner/src/compile
                #:run-system)
  (:export
   #:find-project
   #:project-at
   #:run-project
   #:test-project
   #:call-with-program-argv))

(in-package :clef-runner/src/project)

;;;; Golden-path projects: what `clef run' and `clef test' do with no target.
;;;;
;;;; A project is what `clef new' scaffolds -- init.lisp at the root beside
;;;; exactly one .asd. Both commands load the init (the hermetic source
;;;; registry, the ocicl runtime), then the system, with every compile
;;;; diagnostic rendered the way `clef run FILE' renders it. Only then does
;;;; either call into the user's code: the entry point named on the stub, or
;;;; RUN-TESTS in the test module the stub's test-op wires.
;;;;
;;;; The project loads into THIS image, not a child sbcl. That is fast and
;;;; needs neither sbcl on PATH nor a recent ASDF anywhere but here, and its
;;;; costs are recorded in docs/golden-path/entry-points.md.

;;; ---------------------------------------------------------------------------
;;; Finding the project
;;; ---------------------------------------------------------------------------

(defun project-at (dir)
  "(values system-name asd) when DIR is a golden-path project root: it holds
init.lisp and exactly one .asd. Exactly one, because the name of that file is
the name of the system, and two candidates would mean guessing."
  (let ((dir (uiop:ensure-directory-pathname dir)))
    (when (probe-file (merge-pathnames "init.lisp" dir))
      (let ((asds (uiop:directory-files dir "*.asd")))
        (when (= (length asds) 1)
          (values (pathname-name (first asds)) (first asds)))))))

(defun repository-root-p (dir)
  "Does DIR carry a .git? A directory or a file -- worktrees use a file."
  (probe-file (merge-pathnames ".git" (uiop:ensure-directory-pathname dir))))

(defun find-project (&optional (start (uiop:getcwd)))
  "The nearest golden-path project at or above START, as (values root name),
or NIL.

Walks upward so a command run from src/ finds the project, the same way
init.lisp anchors on its own directory rather than the cwd. The walk stops
after a directory holding .git: a project's own repository is as far as it
makes sense to look, and continuing past it would find someone else's."
  (loop with dir = (uiop:ensure-directory-pathname start)
        do (let ((name (project-at dir)))
             (when name (return (values dir name))))
           (when (repository-root-p dir) (return nil))
           (let ((parent (uiop:pathname-parent-directory-pathname dir)))
             (when (uiop:pathname-equal parent dir) (return nil))
             (setf dir parent))))

;;; ---------------------------------------------------------------------------
;;; Loading
;;; ---------------------------------------------------------------------------

(defun load-init (root)
  "Load the project's init.lisp, as --userinit would.

It anchors its source registry on *LOAD-TRUENAME*, so a plain LOAD from here
behaves exactly like the REPL invocation the project documents. In this image
its (require :asdf) is a no-op and its INITIALIZE-SOURCE-REGISTRY replaces the
runner's own, which is what a hermetic run wants."
  (load (merge-pathnames "init.lisp" root) :verbose nil :print nil))

(defun project-systems (name)
  "A predicate on system names: is this one of the project's own?

The primary system and every inferred subsystem of it -- <name>/src/main and
so on -- but not the vendored dependencies, which are other primaries. Handed to
ASDF as :FORCE so the project's files are compiled afresh on every run: a
warning that appeared once and then vanished because the fasl was now up to
date would make --werror mean different things on consecutive runs. The
dependencies keep their fasls; compiling those again would cost seconds for
nothing.

Passed as a predicate rather than :FORCE T because ASDF compares T against
the exact system name, and in a package-inferred layout the primary owns no
files of its own."
  (lambda (system-name)
    (equal (asdf:primary-system-name system-name) name)))

(defun call-in-one-session (thunk)
  "Call THUNK inside a single ASDF session.

Every ASDF:LOAD-SYSTEM outside a session opens one of its own, and a known
ASDF bug (w3-migration-trial.md §2.2.8) makes each fresh session re-register a
package-inferred system, dropping its load stamps: the .asd is read again and
every file reloaded, with a 'redefining' style-warning for each. Within one
session an action is performed once. ASDF requires every load in the session
to pass the same :FORCE, so callers build the predicate once."
  (asdf/session:with-asdf-session () (funcall thunk)))

(defun call-with-program-argv (name argv thunk)
  "Call THUNK with the program's command line set to NAME followed by ARGV.

The program reads UIOP:*COMMAND-LINE-ARGUMENTS* -- the list after `--' -- the
same way an executable built by ASDF's program-op would, so an entry point
written for `clef run' needs no change when it becomes a binary."
  (let ((sb-ext:*posix-argv* (cons name argv))
        (uiop:*command-line-arguments* argv))
    (funcall thunk)))

;;; ---------------------------------------------------------------------------
;;; Naming functions from the stub
;;; ---------------------------------------------------------------------------

(defun parse-designator (designator)
  "(values package-name symbol-name) for \"pkg:sym\", \"pkg::sym\" or a symbol."
  (if (symbolp designator)
      (values (package-name (symbol-package designator)) (symbol-name designator))
      (let* ((string (string-trim " " designator))
             (colon (position #\: string)))
        (if (null colon)
            (values nil (string-upcase string))
            (values (string-upcase (subseq string 0 colon))
                    (string-upcase (string-left-trim ":" (subseq string colon))))))))

(defun named-function (designator what stream)
  "The function DESIGNATOR names, or NIL after saying on STREAM what is missing.
WHAT describes the role, for the message: \"entry point\", \"test function\"."
  (multiple-value-bind (package-name symbol-name) (parse-designator designator)
    (let ((package (and package-name (find-package package-name))))
      (cond
        ((null package-name)
         (format stream "~&error: ~A ~S needs a package prefix, like ~
                         \"myapp/src/main:main\"~%" what designator)
         nil)
        ((null package)
         (format stream "~&error: ~A ~S: no package ~A exists after loading ~
                         the project~%" what designator package-name)
         nil)
        (t
         (let ((symbol (find-symbol symbol-name package)))
           (cond
             ((and symbol (fboundp symbol)) (fdefinition symbol))
             (t (format stream "~&error: ~A ~S: ~A does not define a function ~A~%"
                        what designator (package-name package) symbol-name)
                nil))))))))

(defun default-entry-point (name)
  (format nil "~A/src/main:main" name))

(defun entry-point (system name stream)
  "The stub's :entry-point, or the convention's, announced when assumed."
  ;; Exported from ASDF/SYSTEM, not re-exported by ASDF itself; likewise
  ;; COMPONENT-IN-ORDER-TO below.
  (let ((declared (asdf/system:component-entry-point system)))
    (or declared
        (let ((assumed (default-entry-point name)))
          (format stream "~&note: ~A.asd declares no :entry-point; calling ~A~%"
                  name assumed)
          assumed))))

(defun op-name-p (designator op-name)
  "Is DESIGNATOR (a symbol or string, however the stub spelled it) OP-NAME?"
  (string-equal (string designator) op-name))

(defun test-modules (system name stream)
  "The systems the stub's test-op loads, from its :in-order-to, or the
convention's <name>/test/main, announced when assumed.

Read off the stub rather than run through ASDF:TEST-SYSTEM on purpose. A
test-op is one compilation unit, and SBCL defers undefined-function warnings
to the end of it -- past any handler inside the tests, which is exactly where a
test that compiles something and inspects the warnings needs them. Loading the
module and calling the function is the same work with no deferral."
  (let* ((in-order-to (asdf/component:component-in-order-to system))
         (test-op (find-if (lambda (entry) (op-name-p (first entry) "TEST-OP"))
                           in-order-to))
         (declared (loop for dep in (rest test-op)
                         when (and (consp dep) (op-name-p (first dep) "LOAD-OP"))
                           append (mapcar #'asdf:coerce-name (rest dep)))))
    (or declared
        (let ((assumed (format nil "~A/test/main" name)))
          (format stream "~&note: ~A.asd wires no test-op; loading ~A~%" name assumed)
          (list assumed)))))

;;; ---------------------------------------------------------------------------
;;; Entry points
;;; ---------------------------------------------------------------------------

(defun run-project (root name &key argv)
  "Load the project at ROOT and call its entry point. Returns the exit code.

The entry point's return value is ignored, as program-op ignores it; a program
that wants a specific exit status calls UIOP:QUIT itself. Anything it signals
and nobody handles ends the process through the runtime's debugger hook."
  (let ((stream (diagnostic-stream))
        (force (project-systems name)))
    (load-init root)
    (let ((code (call-in-one-session (lambda () (run-system name :force force)))))
      (if (/= code +exit-success+)
          code
          (let ((entry (named-function (entry-point (asdf:find-system name) name stream)
                                       "entry point" stream)))
            (cond
              ((null entry) +exit-diagnostics+)
              (t (call-with-program-argv name argv entry)
                 +exit-success+)))))))

(defun test-project (root name)
  "Load the project at ROOT, then its test modules, and call RUN-TESTS in each.
Returns the exit code.

RUN-TESTS signalling is what fails the run: the convention's test entry raises
on any failed check, and an unhandled error under the runtime exits 1."
  (let ((stream (diagnostic-stream))
        (force (project-systems name))
        (modules nil))
    (load-init root)
    (let ((code (call-in-one-session
                 (lambda ()
                   (let ((code (run-system name :force force)))
                     (if (/= code +exit-success+)
                         code
                         (progn
                           (setf modules (test-modules (asdf:find-system name) name stream))
                           (loop for module in modules
                                 for code = (run-system module :force force)
                                 unless (= code +exit-success+)
                                   return code
                                 finally (return +exit-success+)))))))))
      (when (/= code +exit-success+)
        (return-from test-project code))
      (let ((runners (loop for module in modules
                           collect (named-function (format nil "~A:run-tests" module)
                                                   "test function" stream))))
        (cond
          ((some #'null runners) +exit-diagnostics+)
          (t (call-with-program-argv name '()
                                     (lambda () (mapc #'funcall runners)))
             +exit-success+))))))
