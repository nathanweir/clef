(defpackage :clef-lsp/src/scaffold
  (:use :cl)
  (:local-nicknames
    (:ppcre :cl-ppcre))
  (:export
   #:*template-files*
   #:load-template-files
   #:new-project))

(in-package :clef-lsp/src/scaffold)

;;;; clef new: scaffold a golden-path project from the bundled template.
;;;;
;;;; The template source of truth is templates/clef/ at the repo root.
;;;; build.lisp reads it into the image at dump time, so the shipped binary
;;;; can scaffold with no repo checkout and no template registration --
;;;; distribution is the whole reason `clef new' exists.
;;;;
;;;; The renderer understands three constructs, borrowed from cl-template's
;;;; syntax when the same files were also served through ocicl's template
;;;; channel (a W5-era arrangement, retired 2026-09-07 once the template came
;;;; to assume clef; see docs/golden-path/entry-points.md):
;;;;
;;;;   <%= @ key %>                    a required parameter
;;;;   <%= (or (@ key) "default") %>   an optional parameter with a default
;;;;   {{app-name}}                    in file NAMES
;;;;
;;;; A construct outside those is left in the output verbatim, where the
;;;; scaffold tests would see it.

(defparameter *template-files* nil
  "Alist of (relative-path . content), set at build time by LOAD-TEMPLATE-FILES.
NIL when running from source; NEW-PROJECT then loads lazily from the repo.")

(defun template-root-from-source ()
  "templates/clef/ located relative to this file, for from-source runs."
  (let ((here #.(or *compile-file-truename* *load-truename*)))
    (merge-pathnames "../../templates/clef/"
                     (uiop:pathname-directory-pathname here))))

(defun load-template-files (&optional (root (template-root-from-source)))
  "Read every template file under ROOT into *TEMPLATE-FILES*."
  ;; TRUENAME, because the from-source root is spelled with ../.. and
  ;; ENOUGH-PATHNAME cannot subtract that from the truenames DIRECTORY-FILES
  ;; returns -- every "relative" path came out absolute, and `clef new' from
  ;; source then tried to write the template over its own sources.
  (let ((root (truename (uiop:ensure-directory-pathname root)))
        (files '()))
    (uiop:collect-sub*directories
     root (constantly t) (constantly t)
     (lambda (dir)
       (dolist (path (uiop:directory-files dir))
         (push path files))))
    (setf *template-files*
          (loop for path in (sort files #'string< :key #'namestring)
                for rel = (uiop:native-namestring (uiop:enough-pathname path root))
                for content = (uiop:read-file-string path)
                collect (cons rel content)))
    (length *template-files*)))

(defun render (content params)
  "CONTENT with the two supported constructs substituted from PARAMS."
  (let ((out content))
    ;; Optional with default: <%= (or (@ key) "default") %>
    (setf out
          (ppcre:regex-replace-all
           "<%= \\(or \\(@ ([a-z-]+)\\) \"([^\"]*)\"\\) %>"
           out
           (lambda (match key default &rest _)
             (declare (ignore match _))
             (or (cdr (assoc key params :test #'string=)) default))
           :simple-calls t))
    ;; Required: <%= @ key %>
    (setf out
          (ppcre:regex-replace-all
           "<%= @ ([a-z-]+) %>"
           out
           (lambda (match key &rest _)
             (declare (ignore _))
             (or (cdr (assoc key params :test #'string=))
                 (error "Template parameter ~A has no value (in ~S)" key match)))
           :simple-calls t))
    out))

(defun valid-app-name-p (name)
  "Names become directories, package names and system names all at once."
  (and (plusp (length name))
       (every (lambda (c) (or (alphanumericp c) (char= c #\-))) name)
       (alpha-char-p (char name 0))))

(defparameter *yours-if-present* '("README.md" ".gitignore")
  "Template files that are not overwritten AND not a collision when the target
directory already has them. Everything else the template writes is code or
configuration the golden path depends on; these two are the user's.")

(defun project-directory (target output-root)
  "TARGET as an absolute directory under OUTPUT-ROOT: a bare name, a relative
or absolute path, or \".\" for OUTPUT-ROOT itself. \".\" components are
dropped so the last component is always the project's name."
  (let* ((root (uiop:ensure-directory-pathname output-root))
         (dir (uiop:ensure-absolute-pathname (uiop:ensure-directory-pathname target) root))
         (components (remove "." (rest (pathname-directory dir)) :test #'equal)))
    (make-pathname :directory (cons :absolute components) :name nil :type nil
                   :defaults dir)))

(defun new-project (target &key params (output-root (uiop:getcwd)))
  "Scaffold a project at TARGET from the bundled template and return its
directory.

TARGET is a directory: a new name, an existing directory, a path, or \".\".
The project's name -- its system, its packages -- is the last path component.
An existing directory is fine; people make the folder first, and a README or
a docs/ tree already in it is left alone. What is refused is a collision: if
any file the template would write already exists, nothing is written and the
error names every such file. PARAMS is an alist of extra template parameters
(\"author\" etc)."
  (unless *template-files*
    (load-template-files))
  (let* ((dest (project-directory target output-root))
         (name (car (last (pathname-directory dest)))))
    (unless (and (stringp name) (valid-app-name-p name))
      (error "~S is not usable as a project name: letters, digits and hyphens ~
              only, starting with a letter. (The name is the directory's own: ~
              ~S.)" name (uiop:native-namestring dest)))
    (when (uiop:file-exists-p (uiop:pathname-parent-directory-pathname dest))
      ;; A file where the parent should be a directory; ensure-directories-exist
      ;; would say something less clear.
      (error "~A is a file, not a directory." (uiop:native-namestring dest)))
    (let* ((params (cons (cons "app-name" name) params))
           (plan (loop for (rel . content) in *template-files*
                       for rel-rendered = (ppcre:regex-replace-all "{{app-name}}" rel name)
                       collect (list (merge-pathnames rel-rendered dest) rel-rendered content)))
           (existing (loop for (path rel) in plan
                           when (probe-file path) collect rel))
           ;; The user's prose and their ignore list are theirs: a README or a
           ;; .gitignore already in the folder (GitHub writes both) is kept,
           ;; and the template's copy is simply not written.
           (kept (intersection existing *yours-if-present* :test #'string=))
           (collisions (set-difference existing kept :test #'string=)))
      (when collisions
        (error "~A already has ~{~A~^, ~} -- refusing to scaffold over ~
                ~[them~;it~:;them~]. Nothing was written."
               (uiop:native-namestring dest) collisions (length collisions)))
      (loop for (path rel content) in plan
            unless (member rel kept :test #'string=)
              do (ensure-directories-exist path)
                 (with-open-file (out path :direction :output :if-exists :error)
                   (write-string (render content params) out)))
      (values dest kept))))
