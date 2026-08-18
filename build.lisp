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

;; Compile into the project-local build/ directory rather than
;; ~/.cache/common-lisp/, matching load.lisp and test/run-tests.lisp so all
;; three entry points agree on where fasls land.
;;
;; Conditional because this script also runs under the nix builder, where the
;; source is a read-only store path and nothing can be written next to it. There
;; we fall through to ASDF's default cache, which is what that build already
;; relies on.
(let ((build-dir (merge-pathnames "build/" *here*)))
  (when (ignore-errors (ensure-directories-exist build-dir) t)
    (asdf:initialize-output-translations
     `(:output-translations
       ((,*here* :**/ :*.*.*) (,*here* "build" :**/ :*.*.*))
       :inherit-configuration))))

;; Keep build chatter off stdout so this is safe to run from a pipe.
(let ((*standard-output* *error-output*))
  (asdf:load-asd (merge-pathnames "clef.asd" *here*))
  (asdf:load-system :clef))

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

;; parser.lisp bakes (asdf:system-relative-pathname :clef "src/parser/...") into
;; the image at compile time, so the nix build has to compile from the source's
;; final store path rather than a scratch copy -- which leaves nowhere next to
;; build.lisp to write to. CLEF_OUTPUT redirects the dump; `just build' leaves it
;; unset and still gets ./clef.
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
