;;;; Build a standalone clef executable.
;;;;
;;;; Run with: sbcl --script build.lisp    (or: just build)
;;;;
;;;; Why a binary: cl-tree-sitter has a `perform :before (prepare-op ...)' that
;;;; shells out to `make' EVERY time the system loads, and that Makefile needs
;;;; pkg-config to locate tree-sitter. That makes every editor launch depend on
;;;; a working C toolchain in whatever environment the editor happens to spawn
;;;; the server with. Dumping an image moves all of that to build time.

(require :asdf)
(require :sb-introspect)
(require :sb-concurrency)
(require :sb-posix)

(defparameter *here*
  (make-pathname :directory (pathname-directory *load-truename*)))

;; Quiet the compiler's running commentary. A clean build emitted 278 lines, of
;; which ~270 were "; compiling file" / "; wrote" / "; compilation finished".
;; Warnings and errors still come through -- these variables only govern
;; progress chatter. (test/run-tests.lisp already does this; load.lisp now does
;; too.)
(setf *compile-verbose* nil
      *compile-print* nil
      *load-verbose* nil
      *load-print* nil)

;; The tree containing every component this build reads. In a checkout that is
;; the repo root, one level above lsp/; the nix fileset reproduces the same
;; shape, so the lookup is identical in both.
(defparameter *source-root*
  (let ((parent (make-pathname :directory (butlast (pathname-directory *here*)))))
    (if (probe-file (merge-pathnames "conditions/clef-conditions.asd" parent))
        parent
        *here*)))

;; Compile into a build/ directory inside the source tree rather than
;; ~/.cache/common-lisp/, matching load.lisp and test/run-tests.lisp so all
;; three entry points agree on where fasls land.
;;
;; Conditional because this script also runs under the nix builder, where the
;; source is a read-only store path and nothing can be written next to it. There
;; we fall through to ASDF's default cache, which is what that build already
;; relies on.
(let ((build-dir (merge-pathnames "build/" *source-root*)))
  (when (ignore-errors (ensure-directories-exist build-dir) t)
    (asdf:initialize-output-translations
     `(:output-translations
       ((,*source-root* :**/ :*.*.*) (,*source-root* "build" :**/ :*.*.*))
       :inherit-configuration))))

;; A from-scratch compile must not share a process with the dump.
;;
;; Compiling the whole tree in-process leaves compiler-generated code objects
;; scattered through the heap in a way a full GC frees but does not compact out
;; of the core: measured 2026-09-06, a from-scratch build dumped a 642 MB
;; binary with only 70 MB of live dynamic space, while the identical image
;; loaded from a warm fasl cache dumped 145 MB. So when the cache is cold,
;; spawn a child of this same script to do the compiling (it exits before any
;; dump), then proceed here by loading the fasls it left behind.
(unless (uiop:getenv "CLEF_BUILD_COMPILE_ONLY")
  (let ((self (or *load-truename* (merge-pathnames "build.lisp" *here*)))
        (sbcl (first sb-ext:*posix-argv*)))
    (format *error-output* "~&; warming the fasl cache in a child process~%")
    (sb-ext:run-program sbcl
                        (list "--noinform" "--non-interactive"
                              "--load" (namestring self))
                        :environment (cons "CLEF_BUILD_COMPILE_ONLY=1"
                                           (sb-ext:posix-environ))
                        :output *error-output*
                        :error *error-output*
                        ;; argv[0] may be a bare "sbcl" resolved via PATH.
                        :search t)))

;; Keep build chatter off stdout so this is safe to run from a pipe.
(let ((*standard-output* *error-output*))
  ;; Sibling components first.
  (let ((sibling (probe-file (merge-pathnames "conditions/clef-conditions.asd"
                                              *source-root*))))
    (when sibling (asdf:load-asd sibling)))
  (asdf:load-asd (merge-pathnames "clef-lsp.asd" *here*))
  (asdf:load-system :clef-lsp)
  ;; The runner rides in the same image: `clef run' dispatches to it by name
  ;; at run time. Deliberately not in :clef-lsp's :depends-on -- from-source
  ;; entry points load only the LSP and keep working unchanged.
  (let ((runner (probe-file (merge-pathnames "runner/clef-runner.asd"
                                             *source-root*))))
    (when runner
      (asdf:load-asd runner)
      (asdf:load-system :clef-runner)))

;; The compile-only child stops here: its job was filling the fasl cache.
(when (uiop:getenv "CLEF_BUILD_COMPILE_ONLY")
  (format *error-output* "~&; fasl cache warmed; child exiting before dump~%")
  (sb-ext:exit :code 0))
  ;; Bake the golden-path template into the image, so `clef new' works with no
  ;; repo checkout and no ocicl template registration -- the distribution
  ;; problem W8-and-a-half exists to solve.
  (let ((n (uiop:symbol-call :clef-scaffold :load-template-files
                             (merge-pathnames "templates/clef/" *source-root*))))
    (format *error-output* "; bundled ~D template file(s)~%" n)))

;;; SBCL records each dlopen'd library by the name it was asked for. Deps here
;;; are requested by bare soname ("libffi.so.8") and only resolve because the
;;; dev shell sets LD_LIBRARY_PATH. On restart SBCL reopens them by that same
;;; bare name, so the dumped image would only run in an environment that
;;; already has LD_LIBRARY_PATH set -- exactly the fragility the binary exists
;;; to remove. Rewrite each recorded name to the absolute path it actually
;;; resolved to, read out of /proc/self/maps.

(defun mapped-library-paths ()
  "Absolute paths of every shared object currently mapped into this process."
  (let ((paths '()))
    (with-open-file (s "/proc/self/maps" :if-does-not-exist nil)
      (when s
        (loop for line = (read-line s nil nil)
              while line
              for slash = (position #\/ line)
              when (and slash (search ".so" line))
                ;; READ-LINE hands back (simple-array character (*)) -- 32 bits
                ;; per character. Paths pinned from those would sit in the dumped
                ;; image as UTF-32, where nix's reference scanner cannot see them:
                ;; it looks for the store hash as contiguous ASCII. The libraries
                ;; would then be undetected runtime deps of the built binary and a
                ;; GC could collect them out from under it. Store as base-strings
                ;; so the paths are ASCII in the heap and get found.
                do (pushnew (coerce (subseq line slash) 'simple-base-string)
                            paths :test #'string=))))
    paths))

(defun absolutize-shared-objects ()
  "Point every recorded shared object at an absolute path."
  (let ((mapped (mapped-library-paths))
        (fixed 0)
        (unresolved '()))
    (dolist (so sb-alien::*shared-objects*)
      (let ((ns (sb-alien::shared-object-namestring so)))
        (when (and ns (plusp (length ns)) (char/= (char ns 0) #\/))
          ;; "libffi.so.8" must match a mapped "/nix/store/.../libffi.so.8.1.4",
          ;; so accept an exact basename or a versioned suffix of it.
          (let ((hit (find-if (lambda (p)
                                (let ((base (file-namestring p)))
                                  (or (string= base ns)
                                      (and (> (length base) (length ns))
                                           (string= ns (subseq base 0 (length ns)))
                                           (char= (char base (length ns)) #\.)))))
                              mapped)))
            (cond (hit
                   (setf (sb-alien::shared-object-namestring so) hit
                         (sb-alien::shared-object-pathname so) (pathname hit))
                   (incf fixed)
                   (format *error-output* "  pinned ~A -> ~A~%" ns hit))
                  (t (push ns unresolved)))))))
    (format *error-output* "~&Pinned ~D shared object(s) to absolute paths.~%" fixed)
    (when unresolved
      (format *error-output* "WARNING: could not resolve: ~{~A~^, ~}~%" unresolved)
      (format *error-output* "The binary will need LD_LIBRARY_PATH for those.~%"))))

(absolutize-shared-objects)

;; handle-initialize still calls asdf:load-system on the *user's* workspace, so
;; ASDF must re-read its source registry and output translations at runtime
;; instead of reusing whatever the build environment had. This is the hook ASDF
;; expects a dumping image to call; save-lisp-and-die does not run it for us.
(uiop:call-image-dump-hook)

;; parser.lisp bakes (asdf:system-relative-pathname :clef-lsp "src/parser/...") into
;; the image at compile time, so the nix build has to compile from the source's
;; final store path rather than a scratch copy -- which leaves nowhere next to
;; build.lisp to write to. CLEF_OUTPUT redirects the dump; `just build' leaves it
;; unset and still gets ./clef.
;; Collect the compiler's leavings before dumping. This is not optional
;; hygiene: SAVE-LISP-AND-DIE's own collection left ~480 MB of tenured
;; compilation garbage in the heap after a full from-scratch build, and the
;; binary ballooned from 145 MB to 624 MB. An explicit full GC reclaims it.
;; Measured 2026-09-06 while adding the runner to the image.
(sb-ext:gc :full t)
(format *error-output* "~&; live heap at dump: ~D MB~%"
        (floor (sb-kernel:dynamic-usage) (* 1024 1024)))

(let ((out (or (uiop:getenv "CLEF_OUTPUT")
               (merge-pathnames "clef" *here*))))
  (format *error-output* "~&Dumping executable to ~A~%" out)
  (sb-ext:save-lisp-and-die
    out
    :executable t
    :toplevel #'clef-root:main
    ;; Keep the runtime from interpreting the editor's argv as SBCL options,
    ;; and preserve the dumped dynamic-space size.
    :save-runtime-options t))
