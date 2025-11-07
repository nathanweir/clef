(in-package :clef-symbols)

(defun filter-files (file-paths)
       "Filters out files from a list of paths to .lisp files that fit certain criteria"
       ;; Exclude any files under '.direnv'. Long-term we'd achieve this by processing the
       ;; .gitignore
       (remove-if-not (lambda (path)
                              ;; Limit to only util.lisp for now
                              (cl-ppcre:scan "/home/nathan/dev/clef/src/util\\.lisp" (namestring path)))
                      ;; (cl-ppcre:scan "\\.direnv" (namestring path)))
                      file-paths))

(defun build-symbol-map (project-root)
       (slog :debug "Building symbol map at ~A" project-root)
       ;; Discover every .lisp file recursively under the root
       (let* ((wildcard-path (concatenate 'string
                                          (clef-util:cleanup-path project-root)
                                          "/**/*.lisp"))
              (lisp-files (uiop:directory* wildcard-path))
              (filtered-files (filter-files lisp-files)))
             (slog :debug "Found ~A valid Lisp files in workspace." (length filtered-files))
             ;; Process each file to extract symbols
             (dolist (file-path filtered-files)
                     (let ((file-source (clef-util:read-file-text (namestring file-path))))
                          (process-file (namestring file-path) file-source)))))

(defparameter *package-name* nil "The name of the current package encountered when processing the file")

(defun process-file (file-path file-source)
       (slog :debug "Processing file for symbol-map: ~A" file-path)

       ;; Reset any previously found package names
       (setf *package-name* nil)

       ;; Parse the file with tree-sitter and then walk the output tree to find
       ;; the current package, record symbol definitions, symbol references, and
       ;; lexical scopes
       (let ((tree (clef-parser/parser:parse-string file-source)))
            (labels ((walk (n)
                           (let ((type (ts:node-type n)))
                                (when (or (eql type :error) (eql type :missing))
                                      ;; TODO: What to do on syntax errors? Just abort?
                                      '())
                                ;; (push (cons type (ts:node-range n)) results))
                                (progn
                                  ;; (slog :debug "build-symbol-map> node-type is: ~A" type)
                                  (check-for-in-package n file-source)
                                  (check-for-defun n file-source)
                                  (dolist (child (ts:node-children n))
                                          (walk child))))))
                    (walk tree))
            '())
       (slog :debug "package name found was: ~A" *package-name*))

(defun node-obj (node source)
       (let* ((text (clef-parser/parser:node-text node source))
              (object (read-from-string text)))
             object))

(defun check-for-in-package (node source)
       "Checks if the given node is an in-package declaration and updates *package-name* if so"
       (let ((text (clef-parser/parser:node-text node source)))
            ;; (slog :debug "text is: ~A" text)
            ;; Look for (in-package ...) forms
            (when (and (eq (ts:node-type node) :LIST-LIT)
                       (search "in-package" text))
                  (handler-case
                    (let ((form (read-from-string text)))
                         (when (and (consp form)
                                    (eq (car form) 'in-package))
                               (setf *package-name* (second form))))
                    (error () nil)))))

;; TODO: Check for the following types of definition nodes:
;; DEFUN, DEFMACRO, DEFPARAMETER, DESTRUCTRING-BIND
;; Also, LET, LET*, FLET
;; Others to consider in the future (?): DEFTYPE, DEFSPECIAL, DEFSTRUCT, DEFMETHOD, DEFCLASS

;; 'defun', 'defmacro', 'defgeneric', 'defmethod'
(defun check-for-defun (node source)
       "If a 'defun' node is found, unpack the specific type of node, name, and params into
symbol-definitions"
       (when (eq (ts:node-type node) :defun)
             (let* ((obj (node-obj node source))
                    (defun-type (first obj))
                    (symbol-text (second obj))
                    (args (third obj)))
                   (if (string= defun-type "LAMBDA")
                       ;; TODO: handle lambdas separately as we have to process their params
                       (slog :debug "lambda definition")
                       (slog :debug "Found ~A definition for symbol ~A with args ~A"
                             defun-type symbol-text args)))))
