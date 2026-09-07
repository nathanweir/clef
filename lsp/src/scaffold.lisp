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
;;;; The template source of truth is templates/clef/ at the repo root, which is
;;;; ALSO served through ocicl's template search path (`ocicl new app clef`).
;;;; Two delivery channels, one set of files: build.lisp reads them into the
;;;; image at dump time, so the shipped binary can scaffold with no repo, no
;;;; registration and no ocicl template config -- distribution is the whole
;;;; reason `clef new` exists.
;;;;
;;;; Because both channels render the same files, the template may only use
;;;; the subset of cl-template syntax this renderer also understands:
;;;;
;;;;   <%= @ key %>                    a required parameter
;;;;   <%= (or (@ key) "default") %>   an optional parameter with a default
;;;;   {{app-name}}                    in file NAMES
;;;;
;;;; Anything else is an error at template-load time, not a silent divergence
;;;; between the two channels.

(defparameter *template-files* nil
  "Alist of (relative-path . content), set at build time by LOAD-TEMPLATE-FILES.
NIL when running from source; NEW-PROJECT then loads lazily from the repo.")

(defparameter *supported-syntax*
  '("<%= @ [a-z-]+ %>" "<%= \\(or \\(@ [a-z-]+\\) \"[^\"]*\"\\) %>")
  "The only template constructs the embedded renderer handles.")

(defun template-root-from-source ()
  "templates/clef/ located relative to this file, for from-source runs."
  (let ((here #.(or *compile-file-truename* *load-truename*)))
    (merge-pathnames "../../templates/clef/"
                     (uiop:pathname-directory-pathname here))))

(defun check-template-syntax (relative-path content)
  "Fail loudly on template syntax the embedded renderer does not support."
  (let ((stripped content))
    (dolist (pattern *supported-syntax*)
      (setf stripped (ppcre:regex-replace-all pattern stripped "")))
    (when (search "<%" stripped)
      (error "Template ~A uses syntax the bundled renderer does not support.~%~
              Keep templates/clef/ to: <%= @ key %> and (or (@ key) \"default\")."
             relative-path))))

(defun load-template-files (&optional (root (template-root-from-source)))
  "Read every template file under ROOT into *TEMPLATE-FILES*."
  (let ((root (uiop:ensure-directory-pathname root))
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
                do (check-template-syntax rel content)
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

(defun new-project (name &key params (output-root (uiop:getcwd)))
  "Scaffold NAME under OUTPUT-ROOT from the bundled template.
PARAMS is an alist of extra template parameters (\"author\" etc).
Returns the project directory. Signals on any problem; refuses to overwrite."
  (unless (valid-app-name-p name)
    (error "~S is not usable as a project name: letters, digits and hyphens ~
            only, starting with a letter." name))
  (unless *template-files*
    (load-template-files))
  (let* ((params (cons (cons "app-name" name) params))
         (dest (merge-pathnames (make-pathname :directory (list :relative name))
                                (uiop:ensure-directory-pathname output-root))))
    (when (probe-file dest)
      (error "~A already exists -- refusing to scaffold over it."
             (uiop:native-namestring dest)))
    (dolist (entry *template-files*)
      (destructuring-bind (rel . content) entry
        (let* ((rel-rendered (ppcre:regex-replace-all "{{app-name}}" rel name))
               (target (merge-pathnames rel-rendered dest)))
          (ensure-directories-exist target)
          (with-open-file (out target :direction :output :if-exists :error)
            (write-string (render content params) out)))))
    dest))
