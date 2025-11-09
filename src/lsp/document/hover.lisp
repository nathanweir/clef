(in-package :clef-lsp/document)

;; (declaim (ftype (function (cons) null) ignore-msg))
(declaim (ftype (function (string integer integer) string) find-symbol-at-position))
(defun find-symbol-at-position (document-text line char)
       "Finds the symbol within some text at a given position"
       (let* ((tree (clef-parser/parser:parse-string document-text))
              (result nil))
             (labels ((position-in-range-p (node)
                                           (multiple-value-bind (start-line start-char end-line end-char)
                                                                (clef-parser/parser:node-range node)
                                                                ;; (slog :debug "vals: ~A ~A ~A ~A" start-line start-char end-line end-char)
                                                                (or
                                                                  ;; Same line, char in range
                                                                  (and (= line start-line) (= line end-line)
                                                                       (<= start-char char) (< char end-char))
                                                                  ;; Multi-line node
                                                                  (and (or (> line start-line)
                                                                           (and (= line start-line) (>= char start-char)))
                                                                       (or (< line end-line)
                                                                           (and (= line end-line) (< char end-char)))))))
                      (visit-node (node)
                                  ;; (slog :debug "node type: ~A" (ts:node-type node))
                                  (when (and (and (consp (ts:node-type node)))
                                             (eq (second (ts:node-type node)) :SYM-LIT)
                                             (position-in-range-p node))
                                        (setf result node)
                                        (return-from visit-node))
                                  (dolist (child (ts:node-children node))
                                          (when (not result) (visit-node child)))))
                     (when tree (visit-node tree))
                     (when result
                           (clef-parser/parser:node-text result document-text)))))

;; (defun get-hover-doc (symbol &optional (package "cl-user"))
;;        "Creates hover doc text (Markdown) for a given symbol and optional package name"
;;        (let ((found-symbol (find-symbol (string-upcase symbol))))
;;             (slog :debug "~A > ~A" symbol found-symbol)
;;             (documentation (read-from-string symbol) 'function)))

(defun handle-text-document-hover (message)
       ;; (slog :debug "hover: ~A" message)
       ;; (slog :debug "~A" (clef-util:shallow-hash-vals message))
       (let* ((params (clef-jsonrpc/types:request-params message))
              (document-uri (href params "text-document" "uri"))
              (hover-line (href params "position" "line"))
              (hover-char (href params "position" "character"))
              (symbol-at-pos (find-symbol-at-position
                               (href clef-lsp/server:*documents* document-uri)
                               hover-line
                               hover-char))
              (document-text (href clef-lsp/server:*documents* document-uri))
              (tree (clef-parser/parser:parse-string document-text))
              (symbol-pkg (or (clef-parser/utils:find-package-declaration tree document-text)
                              *package*))
              ;; TODO: Get current package
              ;; TODO: Figure out why defun and some other symbols aren't being found
              ;; Or, is it actually necessary? It should work OK with no package name,
              ;; but some values don't show hover when removed. Not sure why
              (doc (get-symbol-doc symbol-at-pos (package-name symbol-pkg))))
             (slog :debug "hover doc ~A" doc)
             ;; (slog :debug "is stringp ~A" (stringp doc))
             (dict "contents"
                   (if (not (stringp doc))
                       #()
                       (format-hover-text doc)))))
;; (format-hover-text (cl-ppcre:regex-replace-all "\\n" doc " "))))))
;; (dict "contents" (parse-symbol-description-to-markdown doc))))
;;              (dict "contents" (dict "kind" "markdown"
;;                                     "value" "
;; ```lisp
;; (defun find-symbol-at-position ;; STRING &REST T
;;     (document-text ;; string
;;      line          ;; number
;;      char)         ;; number
;; ```
;; Finds the symbol within some text at a given position

;; ---

;; CLEF-LSP/DOCUMENT::`FIND-SYMBOL-AT-POSITION` [symbol]

;; _/home/nathan/dev/clef/src/lsp/document/hover.lisp_"))))
;; "value" "<h1>hello <p>stuff</p></h1>"))))
;;
;; (slog :debug "sources-by-name: ~A"
;;       (sb-introspect:find-definition-sources-by-name 'mapcar :function))
;; (dict "contents" (format nil "Hover at line ~A, char ~A in document ~A, symbol is ~A"
;;                          hover-line hover-char document-uri symbol-at-pos))))

;; From Alive

(defun get-symbol-doc (name &optional pkg-name)
       (let* ((sym (lookup-symbol name pkg-name)))
             (when sym
                   (with-output-to-string (str)
                                          (cond ((get-lambda-list name pkg-name)
                                                 (describe sym str))
                                                ((boundp sym)
                                                 (format str "~A~%" (symbol-value sym)))
                                                (T (describe sym str)))))))

(defun lookup-symbol (name &optional pkg-name)
       (let ((pkg (if pkg-name
                      (find-package (string-upcase pkg-name))
                      *package*))
             (sym-name (if (and name
                                (char= #\| (char name 0))
                                (char= #\| (char name (1- (length name)))))
                           (subseq name 1 (- (length name) 1))
                           name)))
            (if (and sym-name pkg)
                (or (find-symbol sym-name pkg)
                    (find-symbol (string-upcase sym-name) pkg))
                (values nil nil))))

(defun get-lambda-list (fn-name &optional pkg-name)
       (ignore-errors (sb-introspect:function-lambda-list
                        (lookup-symbol fn-name pkg-name))))

(defparameter *full-name-regex* "(.*)(?:.|\\n)*(?:\\S+)\\s+names a compiled function")
;; (defparameter *full-name-regex* "(.*)\\s+(?:\\S+)\\s+names a compiled function")
(defparameter *name-regex* "(\\S+) names a compiled function")
(defparameter *params-regex* "Lambda-list:\\s+\\((.*?)\\)")
(defparameter *types-regex* "Declared\\stype:\\s+\\(FUNCTION\\s+\\((.*?)\\)\\s+\\(VALUES\\s+(.*?)\\)")
(defparameter *doc-regex* "Documentation:\\s+(.*?)\\s+Source")
(defparameter *file-regex* "Source\\s+file:\\s+(.*)\\s*")

;; (declaim (ftype (function (string) string) extract-doc-parts))
;; (defun extract-doc-parts (text)
;;        (second (cl-ppcre:scan-to-strings *parts-regex* text)))

;; (defun get-desc-body (text)
;;        (let* ((lines (cl-ppcre:split #\Newline text))
;;               (full-symbol-name (first lines)))
;;              (second lines)))

(declaim (ftype (function (string string) string) get-params-code))
(defun get-params-code (params-text params-type-text)
       "Formats to (param1 ;; type1 [\n] param2 ;; type2 [\n] ...paramN ;; typeN)"
       (when (string= params-text "")
             (return-from get-params-code "()"))
       (let* ((params-list (cl-ppcre:split #\Space params-text))
              (type-list (if (string= params-type-text "")
                             ;; Handles the case where we have no type info; just show 'T' as the type
                             (make-list (length params-list) :initial-element "T")
                             (cl-ppcre:split #\Space params-type-text)))
              ;; Calculate longest param name length so we know how far to space hover
              ;; the type comments
              (longest-len (apply #'max (mapcar #'length params-list))))
             (when (= (length params-list) 1)
                   (return-from get-params-code
                                (format nil "    (~A) ;; ~A" (string-downcase (first params-list)) (first type-list))))
             (loop :for param in params-list
                   :for type in type-list
                   :for i from 1
                   :collect (format nil "    ~A~A~A~vT;; ~A"
                                    (if (= i 1) "(" " ")
                                    (string-downcase param)
                                    (if (= i (length params-list)) ")" "")
                                    (+ (if (= i (length params-list)) 6 7) longest-len)
                                    type)
                   :into output
                   :finally (return (format nil "~{~A~^~%~}" output)))))


(defun regex-desc-parts (body-text)
       ;; (format t ">>>> ~A~%" (second (multiple-value-list (cl-ppcre:scan-to-strings "(GET)" body-text))))
       (flet ((maybe-aref (vec idx)
                          (when (and vec (vectorp vec) (> (length vec) idx))
                                (aref vec idx))))
             (let*  ((full-name-m   (multiple-value-list (cl-ppcre:scan-to-strings *full-name-regex* body-text)))
                     (function-name-m (multiple-value-list (cl-ppcre:scan-to-strings *name-regex* body-text)))
                     (params-m      (multiple-value-list (cl-ppcre:scan-to-strings *params-regex* body-text)))
                     (types-m       (multiple-value-list (cl-ppcre:scan-to-strings *types-regex* body-text)))
                     (doc-m         (multiple-value-list (cl-ppcre:scan-to-strings *doc-regex* body-text)))
                     (file-m        (multiple-value-list (cl-ppcre:scan-to-strings *file-regex* body-text)))
                     (full-name     (maybe-aref (second full-name-m) 0))
                     (function-name (maybe-aref (second function-name-m) 0))
                     (params-text   (maybe-aref (second params-m) 0))
                     (params-type-text (maybe-aref (second types-m) 0))
                     (ret-types     (maybe-aref (second types-m) 1))
                     (description   (maybe-aref (second doc-m) 0))
                     (source-file   (maybe-aref (second file-m) 0)))
                    ;; (slog :debug "==>~A" full-name-m)
                    ;; (slog :debug "extracted parts: ~%full-name: ~A,~%function-name: ~A,~%params-text: ~A,~%params-type-text: ~A,~%ret-types: ~A,~%description: ~A~%source-file: ~A"
                    ;;       full-name function-name params-text params-type-text ret-types description source-file)
                    (list full-name function-name params-text params-type-text ret-types description source-file))))


;; (declaim (ftype (function (string) (or string nil)) format-hover-text))
(defun format-hover-text (text)
       "Formats hover text (Markdown) from symbol description text"
       ;; (slog :debug "format-hover-text input: ~A" text)
       ;; Only support functions for now
       (when (eq (search "names a compiled function" text) nil)
             (slog :debug "no function found in hover text")
             (return-from format-hover-text nil))

       (destructuring-bind (full-name function-name params-text params-type-text ret-types description source-file)
                           (regex-desc-parts text)
                           (let* ((backticked-full-name (cl-ppcre:regex-replace-all function-name full-name (format nil "`~A`" function-name)))
                                  (params-code (get-params-code params-text (if params-type-text params-type-text "")))
                                  (output (format nil "```lisp~%(defun ~A ;; => ~A~%~A~%```~%~%~A~%~%---~%~A~%~%*~A*"
                                                  (string-downcase function-name)
                                                  (if ret-types ret-types "T")
                                                  params-code
                                                  (if description description "")
                                                  backticked-full-name
                                                  ;; Not sure why, but this source-file consistently has a space at the end
                                                  (string-trim '(#\Space #\Tab #\Newline) source-file))))
                                 (format nil "~A~%" output))))
