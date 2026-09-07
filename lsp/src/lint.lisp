(in-package :clef-lint)

;;;; The convention linter.
;;;;
;;;; Checks the golden-path package convention (docs/surveys/w3-packages.md):
;;;; one package per file named by its project-relative path, no :USE except
;;;; :CL, dependencies declared where they are used. Every rule here was
;;;; measured before it was written -- each catches a failure that otherwise
;;;; surfaces late, elsewhere, or in ASDF's vocabulary:
;;;;
;;;;   :lint-no-defpackage      package-inferred errors at LOAD time, naming the
;;;;                            system, not the file
;;;;   :lint-name-mismatch      SILENT alone; fails one step removed, in the
;;;;                            consumer, as "does not designate any package"
;;;;                            with no pointer to the cause (survey E2a/E2b)
;;;;   :lint-use-discipline     :USE inherits an unbounded symbol set and hides
;;;;                            the dependency edge among inherited names
;;;;   :lint-import-cycle       ASDF reports cycles as an op-object dump
;;;;                            (survey E2c); this names files and lines
;;;;
;;;; Extraction is by tree-sitter, not READ: robust on files that do not parse
;;;; whole, immune to reader macros, and positions come for free. Findings are
;;;; CLEF-CONDITIONS:DIAGNOSTIC structs rendered by the same renderer clef-run
;;;; uses, so lint output and runtime diagnostics read as one tool.

;; Lint positions come from the parse tree and are exact; without this the
;; renderer appends "(location is the enclosing form; exact position
;; unavailable)" to every finding, which would be a lie.
(dolist (kind '(:lint-no-defpackage :lint-name-mismatch
                :lint-use-discipline :lint-import-cycle))
  (pushnew kind clef-conditions:*exactly-located-kinds*))

;;; ---------------------------------------------------------------------------
;;; Reading defpackage forms out of a parse tree
;;; ---------------------------------------------------------------------------

(defun node-kind (node)
  (let ((k (ts:node-type node)))
    (if (consp k) (second k) k)))

(defun elements (node)
  "NODE's children minus comments."
  (remove :comment (ts:node-children node) :key #'node-kind))

(defun node-source-text (node source)
  (ignore-errors (clef-parser/parser:node-text node source)))

(defun package-designator-name (text)
  "The package name in TEXT, however it was spelled: :foo, #:foo, foo, \"FOO\".
Lowercased for comparison; NIL when TEXT does not look like a designator."
  (when (and text (plusp (length text)))
    (let ((trimmed (string-left-trim "#:" text)))
      (cond
        ((zerop (length trimmed)) nil)
        ((char= (char trimmed 0) #\")
         (when (>= (length trimmed) 2)
           (string-downcase (string-trim "\"" trimmed))))
        (t (string-downcase trimmed))))))

(defstruct (decl (:conc-name decl-))
  "One file's leading defpackage, as the linter sees it."
  file            ; pathname
  source          ; full file text
  node            ; the defpackage form's node, or NIL when missing
  name            ; declared package name, lowercased, or NIL
  name-node       ; node of the name, for pointing at it
  uses            ; list of (name . node) from :use
  deps)           ; list of (name . node) from every dependency clause

(defparameter +dependency-clauses+
  '("import-from" "shadowing-import-from" "mix" "use-reexport" "mix-reexport"
    "reexport")
  "Clauses whose (first) arguments name dependency packages, matching what
ASDF's PACKAGE-DEPENDENCIES reads. :USE and :LOCAL-NICKNAMES are handled
separately -- :USE has its own rule, and nicknames name deps per entry.")

(defun clause-key (clause-node source)
  "The keyword naming CLAUSE-NODE's role, lowercased and colon-free, or NIL."
  (let ((head (first (elements clause-node))))
    (when (and head (eq (node-kind head) :kwd-lit))
      (string-downcase
       (string-left-trim ":" (or (node-source-text head source) ""))))))

(defun parse-defpackage (form-node source)
  "(values name name-node uses deps) from a defpackage FORM-NODE."
  (let* ((els (elements form-node))
         (name-node (second els))
         (name (package-designator-name (node-source-text name-node source)))
         (uses '())
         (deps '()))
    (dolist (clause (cddr els))
      (when (eq (node-kind clause) :list-lit)
        (let ((key (clause-key clause source))
              (args (rest (elements clause))))
          (cond
            ((null key))
            ((string= key "use")
             (dolist (arg args)
               (let ((n (package-designator-name (node-source-text arg source))))
                 (when n
                   (push (cons n arg) uses)
                   (push (cons n arg) deps)))))
            ((string= key "local-nicknames")
             (dolist (entry args)
               (when (eq (node-kind entry) :list-lit)
                 (let* ((pair (elements entry))
                        (actual (second pair))
                        (n (package-designator-name
                            (node-source-text actual source))))
                   (when n (push (cons n actual) deps))))))
            ((member key +dependency-clauses+ :test #'string=)
             (let* ((target (first args))
                    (n (package-designator-name
                        (node-source-text target source))))
               (when n (push (cons n target) deps))
               ;; :mix and the reexports name EVERY argument.
               (unless (member key '("import-from" "shadowing-import-from")
                               :test #'string=)
                 (dolist (arg (rest args))
                   (let ((m (package-designator-name
                             (node-source-text arg source))))
                     (when m (push (cons m arg) deps)))))))))))
    (values name name-node (nreverse uses) (nreverse deps))))

(defun defpackage-head-p (node source)
  ;; A bare symbol parses as :sym-lit; a package-qualified one such as
  ;; uiop:define-package parses as :package-lit wrapping two :sym-lits. Found
  ;; by the migration trial: the first define-package facade in the repo was
  ;; reported as having no defpackage at all.
  (and (eq (node-kind node) :list-lit)
       (let* ((head (first (elements node)))
              (text (and head
                         (member (node-kind head) '(:sym-lit :package-lit))
                         (node-source-text head source))))
         (and text
              (member (string-downcase text)
                      '("defpackage" "uiop:define-package" "define-package")
                      :test #'string=)))))

(defun read-decl (file)
  "The leading defpackage of FILE as a DECL. NODE is NIL when there is none."
  (let* ((source (uiop:read-file-string file))
         (tree (clef-parser/parser:parse-string source))
         (first-form (find-if (lambda (n)
                                (not (member (node-kind n) '(:comment))))
                              (ts:node-children tree))))
    (if (and first-form (defpackage-head-p first-form source))
        (multiple-value-bind (name name-node uses deps)
            (parse-defpackage first-form source)
          (make-decl :file file :source source :node first-form
                     :name name :name-node name-node :uses uses :deps deps))
        (make-decl :file file :source source :node nil))))

;;; ---------------------------------------------------------------------------
;;; Findings
;;; ---------------------------------------------------------------------------

(defun byte-offset-of (node source)
  "Byte offset of NODE's start in SOURCE, via its row/column."
  (when node
    (let ((row (clef-parser/parser:node-start-point-row node))
          (col (clef-parser/parser:node-start-point-column node))
          (offset 0))
      (dotimes (i row)
        (declare (ignore i))
        (let ((nl (position #\Newline source :start offset)))
          (unless nl (return-from byte-offset-of nil))
          (setf offset (1+ nl))))
      (+ offset col))))

(defun finding (decl node kind severity message &rest args)
  (clef-conditions:make-diagnostic
   :severity severity
   :kind kind
   :message (apply #'format nil message args)
   :file (uiop:native-namestring (decl-file decl))
   :file-position (or (byte-offset-of node (decl-source decl)) 0)))

(defun expected-package-name (system-name relative-path)
  "What the convention says RELATIVE-PATH's package must be called."
  (format nil "~A/~A"
          (string-downcase system-name)
          (string-downcase
           ;; Shed the .lisp; keep directory structure.
           (let ((n (uiop:native-namestring relative-path)))
             (if (uiop:string-suffix-p n ".lisp")
                 (subseq n 0 (- (length n) 5))
                 n)))))

(defun internal-p (name system-name)
  "Does NAME belong to this project's namespace?"
  (let ((prefix (format nil "~A/" (string-downcase system-name))))
    (uiop:string-prefix-p prefix name)))

(defun check-file (decl system-name relative-path)
  "The findings for one file, minus cycle detection (which is global)."
  (let ((findings '()))
    (cond
      ((null (decl-node decl))
       (push (finding decl nil :lint-no-defpackage :warning
                      "No leading DEFPACKAGE. Under package-inferred-system ~
                       this file cannot be loaded as a module at all; the ~
                       error, when it comes, names the system and not the file.")
             findings))
      (t
       ;; The convention's one linter-critical rule: name = path. The failure
       ;; this prevents is silent here and surfaces in a CONSUMER as "does not
       ;; designate any package", pointing away from the cause.
       (let ((expected (expected-package-name system-name relative-path)))
         (unless (and (decl-name decl)
                      (string= (decl-name decl) expected))
           (push (finding decl (or (decl-name-node decl) (decl-node decl))
                          :lint-name-mismatch :warning
                          "Package ~:[name is unreadable~;~:*~A~] but this file's ~
                           path says ~A. Inference maps them positionally, so ~
                           the mismatch is silent here and breaks every ~
                           importer instead."
                          (decl-name decl) expected)
             findings)))
       ;; :USE discipline.
       (dolist (use (decl-uses decl))
         (destructuring-bind (name . node) use
           (unless (member name '("cl" "common-lisp") :test #'string=)
             (push (finding decl node :lint-use-discipline :style-warning
                            ":USE ~:@(~A~) inherits an unbounded set of symbols. ~
                             Name what you take (:import-from) or alias the ~
                             package (:local-nicknames)."
                            name)
                   findings))))))
    (nreverse findings)))

(defun find-cycles (decls system-name)
  "Findings for import cycles among project-internal packages.

Plain DFS with an explicit path, so the finding can NAME the cycle in order --
which is the entire advantage over letting ASDF discover it and print an
op-object dump."
  (let ((by-name (make-hash-table :test #'equal))
        (findings '())
        (reported (make-hash-table :test #'equal)))
    (dolist (d decls)
      (when (decl-name d) (setf (gethash (decl-name d) by-name) d)))
    (labels ((walk (name path)
               (let ((cycle-start (member name path :test #'string=)))
                 (cond
                   (cycle-start
                    ;; PATH is a stack, car most recent; the cycle in visit
                    ;; order runs from NAME through the stacked nodes above its
                    ;; first occurrence and back to NAME.
                    (let* ((chain (cons name (reverse (ldiff path cycle-start))))
                           (key (sort (copy-list chain) #'string<)))
                      (unless (gethash key reported)
                        (setf (gethash key reported) t)
                        (let ((d (gethash name by-name)))
                          (push (finding d (decl-node d) :lint-import-cycle :warning
                                         "Import cycle: ~{~A~^ -> ~} -> ~A. ~
                                          ASDF will refuse to load any of them."
                                         chain name)
                                findings)))))
                   (t
                    (let ((d (gethash name by-name)))
                      (when d
                        (dolist (dep (decl-deps d))
                          (when (internal-p (car dep) system-name)
                            (walk (car dep) (cons name path)))))))))))
      (dolist (d decls)
        (when (decl-name d) (walk (decl-name d) '()))))
    (nreverse findings)))

;;; ---------------------------------------------------------------------------
;;; The project walk, and the CLI
;;; ---------------------------------------------------------------------------

(defparameter +skipped-directories+
  '("ocicl" ".git" "tmp" "build" ".direnv" "result" "node_modules")
  "Never descended into. ocicl/ is vendored third-party code; the convention
binds this project, not its dependencies.")

(defun project-lisp-files (root)
  (let ((files '()))
    (uiop:collect-sub*directories
     (uiop:ensure-directory-pathname root)
     (constantly t)
     (lambda (dir)
       (not (member (first (last (pathname-directory dir)))
                    +skipped-directories+ :test #'equal)))
     (lambda (dir)
       (dolist (f (uiop:directory-files dir "*.lisp"))
         (push f files))))
    (nreverse files)))

(defun package-inferred-system-root-p (root)
  "(values system-name asd-path) when ROOT holds a package-inferred .asd."
  (dolist (asd (uiop:directory-files
                (uiop:ensure-directory-pathname root) "*.asd"))
    (let ((text (uiop:read-file-string asd)))
      (when (search "package-inferred-system" text)
        (return (values (pathname-name asd) asd)))))
  )

(defun reserved-tooling-p (file root)
  "Is FILE a reserved tooling file rather than a module?

The golden-path template puts init.lisp at the project root -- the hermetic
loader, run by --userinit, never loaded as a system component. The convention
reserves that name at the root; everything else that is a .lisp file is a
module and answers to the rules."
  (and (string-equal (pathname-name file) "init")
       (equal (pathname-directory file)
              (pathname-directory (uiop:ensure-directory-pathname root)))))

(defun lint-project (root)
  "Every finding for the project at ROOT, or (values NIL reason) when ROOT is
not a convention project at all."
  (let ((root (uiop:ensure-directory-pathname root)))
    (multiple-value-bind (system-name) (package-inferred-system-root-p root)
      (if (not system-name)
          (values nil :not-a-convention-project)
          (let* ((decls-and-paths
                   (loop for file in (project-lisp-files root)
                         unless (reserved-tooling-p file root)
                           collect (list (read-decl file)
                                         (uiop:enough-pathname file root))))
                 (findings
                   (loop for (decl rel) in decls-and-paths
                         append (check-file decl system-name rel))))
            (values (append findings
                            (find-cycles (mapcar #'first decls-and-paths)
                                         system-name))
                    system-name))))))

(defun main (args)
  "Entry point for `clef lint [DIR]'. Returns an exit code."
  (let ((root (if (first args)
                  (uiop:ensure-absolute-pathname
                   (uiop:ensure-directory-pathname (first args))
                   (uiop:getcwd))
                  (uiop:getcwd))))
    (multiple-value-bind (findings system-name) (lint-project root)
      (cond
        ((eq system-name :not-a-convention-project)
         (format t "clef lint: no package-inferred .asd under ~A -- nothing to ~
                    check.~%(The convention linter binds golden-path projects; ~
                    see `clef new'.)~%"
                 (uiop:native-namestring root))
         0)
        ((null findings)
         (format t "clef lint: ~A is clean.~%" system-name)
         0)
        (t
         (dolist (f findings)
           (clef-conditions:render f)
           (terpri))
         (format t "~D finding~:P.~%" (length findings))
         1)))))
