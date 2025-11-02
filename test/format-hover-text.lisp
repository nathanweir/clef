(require 'asdf)
(asdf:load-system :cl-ppcre)


;; (defparameter *doc-text* "CLEF-LSP/DOCUMENT::FIND-SYMBOL-AT-POSITION [symbol]
;; FIND-SYMBOL-AT-POSITION names a compiled function: Lambda-list: (DOCUMENT-TEXT LINE CHAR) Declared type: (FUNCTION (STRING INTEGER INTEGER) (VALUES STRING &REST T)) Documentation: Finds the symbol within some text at a given position Source file: /home/nathan/dev/clef/src/lsp/document/hover.lisp")

(defparameter *doc-text* "CLEF-LSP/DOCUMENT::GET-PARAMS-CODE   [symbol]   GET-PARAMS-CODE names a compiled function:   Lambda-list: (PARAMS-TEXT PARAMS-TYPE-TEXT)   Declared type: (FUNCTION (STRING STRING) (VALUES STRING &REST T))   Derived type: (FUNCTION (STRING STRING)                  (VALUES SIMPLE-STRING &OPTIONAL))   Documentation:     Formats to (param1 ;; type1nparam2 ;; type2n...paramn ;; typen)   Source file: /home/nathan/dev/clef/src/lsp/document/hover.lisp")

;; (defparameter *parts-regex* "(\\S+).*Lambda-list:\\s+\\((.*?)\\)\\s+Declared\\stype:\\s+\\(FUNCTION\\s+\\((.*?)\\)\\s+\\(VALUES\\s+(.*?)\\).*\\s+Documentation:\\s+(.*?)\\s+Source\\s+file:\\s+(.*)")

;; (defparameter *params-regex* "(\S+).*Lambda-list:\s+\((.*?)\)")
;; (defparameter *types-regex* "Declared\stype:\s+\(FUNCTION\s+\((.*?)\)\s+\(VALUES\s+(.*?)\)")
;; (defparameter *doc-regex* "Documentation:\s+(.*?)\s+Source")
;; (defparameter *file-regex* "Source\s+file:\s+(.*)")
;;
(defparameter *full-name-regex* "(.*)\\s+(?:\\S+)\\s+names a compiled function")
(defparameter *name-regex* "(\\S+) names a compiled function")
(defparameter *params-regex* "Lambda-list:\\s+\\((.*?)\\)")
(defparameter *types-regex* "Declared\\stype:\\s+\\(FUNCTION\\s+\\((.*?)\\)\\s+\\(VALUES\\s+(.*?)\\)")
(defparameter *doc-regex* "Documentation:\\s+(.*?)\\s+Source")
(defparameter *file-regex* "Source\\s+file:\\s+(.*)")

(declaim (ftype (function (string) string) extract-doc-parts))
(defun extract-doc-parts (text)
       (second (cl-ppcre:scan-to-strings *parts-regex* text)))

;; (defun get-desc-body (text)
;;        (let* ((lines (cl-ppcre:split #\Newline text))
;;               (full-symbol-name (first lines)))
;;              (second lines)))

(declaim (ftype (function (string string) string) get-params-code))
(defun get-params-code (params-text params-type-text)
       "Formats to (param1 ;; type1\nparam2 ;; type2\n...paramn ;; typen)"
       (when (string= params-text "")
             (return-from get-params-code "()"))
       (let* ((params-list (cl-ppcre:split #\Space params-text))
              (type-list (cl-ppcre:split #\Space params-type-text))
              ;; Calculate longest param name length so we know how far to space hover
              ;; the type comments
              (longest-len (apply #'max (mapcar #'length params-list))))
             (when (= (length params-list) 1)
                   (return-from get-params-code
                                (format nil "(~A) ;; ~A" (string-downcase (first params-list)) (first type-list))))
             (loop :for param in params-list
                   :for type in type-list
                   :for i from 1
                   :collect (format nil "    ~A~A~A~vT;; ~A"
                                    (if (= i 1) "(" " ")
                                    (string-downcase param)
                                    (if (= i (length params-list)) ")" "")
                                    (+ 6 longest-len)
                                    type)
                   :into output
                   :finally (return (format nil "~{~A~^~%~}" output)))))

(defun regex-desc-parts (body-text)
       ;; (format t ">>>> ~A~%" (second (multiple-value-list (cl-ppcre:scan-to-strings "(GET)" body-text))))

       (let ((full-name (aref (second (multiple-value-list (cl-ppcre:scan-to-strings *full-name-regex* body-text))) 0))
             (function-name (aref (second (multiple-value-list  (cl-ppcre:scan-to-strings *name-regex* body-text))) 0))
             (params-text (aref (second (multiple-value-list (cl-ppcre:scan-to-strings *params-regex* body-text))) 0))
             (params-type-text (aref (second (multiple-value-list (cl-ppcre:scan-to-strings *types-regex* body-text))) 0))
             (ret-types (aref (second (multiple-value-list (cl-ppcre:scan-to-strings *types-regex* body-text))) 1))
             (description (aref (second (multiple-value-list (cl-ppcre:scan-to-strings *doc-regex* body-text))) 0))
             (source-file (aref (second (multiple-value-list (cl-ppcre:scan-to-strings *file-regex* body-text))) 0)))
            ;; (format t "extracted parts: ~%~A,~%~A,~%~A,~%~A,~%~A,~%~A~%"
            ;;         function-name params-text params-type-text ret-types description source-file)
            (list full-name function-name params-text params-type-text ret-types description source-file)))

(defun format-hover-text (text)
       ;; Only support functions for now
       (when (eq (search "names a compiled function" text) nil)
             (format t "no function found in hover text~%")
             (return-from format-hover-text nil))

       (destructuring-bind (full-name function-name params-text params-type-text ret-types description source-file)
                           (regex-desc-parts text)
                           (let* ((backticked-full-name (cl-ppcre:regex-replace-all function-name full-name (format nil "`~A`" function-name)))
                                  (params-code (get-params-code params-text params-type-text))
                                  (output (format nil "```lisp~%(defun ~A ;; => ~A~%~A~%```~%~A~%---~%~A~%__~A__"
                                                  (string-downcase function-name)
                                                  ret-types
                                                  params-code
                                                  description
                                                  backticked-full-name
                                                  source-file)))
                                 (format nil "~A~%" output))))

(format t "~A" (format-hover-text *doc-text*))
