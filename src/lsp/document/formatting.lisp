(in-package :clef-lsp/document)

;; TODO: Remove indentify as it cannot generate incremental text-edit objects needed by the LSP
;; (indentify:load-default-templates)

;; (declaim (ftype (function (state:state cons) hash-table) on-type))
;; (defun on-type (state msg)
;;     (let* ((id (cdr (assoc :id msg)))
;;            (params (cdr (assoc :params msg)))
;;            (doc (cdr (assoc :text-document params)))
;;            (opts (cdr (assoc :options params)))
;;            (pos (cdr (assoc :position params)))
;;            (uri (cdr (assoc :uri doc)))
;;            (text (or (state:get-file-text state uri) ""))
;;            (edits (formatter:on-type (make-string-input-stream text)
;;                                      :options (fmt-opts:convert opts)
;;                                      :pos pos))
;;            (value (if edits
;;                       (fmt-utils:to-text-edits edits)
;;                       (make-array 0))))

;;         (lsp-msg:create-response id :result-value value)))

;; (defun fmt-opts (opts)
;;     (mapcar (lambda (item)
;;                 (cond ((eq :tab-size (car item)) (cons :indent-width (cdr item)))
;;                       (T item)))
;;             opts))

;; (defun to-lsp-range (range)
;;     (let ((lsp-range (make-hash-table :test #'equalp)))

;;         (setf (gethash "start" lsp-range) (alive/range:start range))
;;         (setf (gethash "end" lsp-range) (alive/range:end range))

;;         lsp-range))


;; (defun to-text-edits (edits)
;;     (if (and edits (< 0 (length edits)))
;;         (mapcar (lambda (edit)
;;                     (let ((text-edit (make-hash-table :test #'equalp)))

;;                         (setf (gethash "range" text-edit) (to-lsp-range (alive/text-edit:range edit)))
;;                         (setf (gethash "newText" text-edit) (alive/text-edit:text edit))

;;                         text-edit))
;;                 edits)
;;         (make-array 0)))

;; The above is all adapted from Alive LSP
;;
;; formatting options: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#formattingOptions
;; (defun handle-text-document-formatting (message)
;;     (let* ((params (clef-jsonrpc/types:request-params message))
;;            (file-uri (href params "text-document" "uri"))
;;            (file-text (gethash file-uri clef-lsp/server:*documents*))
;;            ;; (pos (href params "position"))
;;            (options (clef-util:hash-table-to-alist (href params "options"))))
;;         (slog :debug "Formatting document ~A with options ~A" file-uri options)
;;         (let* ((edits (alive/format:on-type (make-string-input-stream file-text)
;;                                             ;; I though this could be null, but crashes if I omit?
;;                                             :pos (alive/position:create 0 0)
;;                                             :options (fmt-opts options)))
;;                (value (if edits
;;                           (to-text-edits edits)
;;                           (make-array 0))))
;;             (slog :debug "Generated ~A edits for formatting request on ~A" (length value) file-uri)
;;             ;; (print-object (first value) *standard-output*)

;;             (slog :debug "First edit is ~A" (first value))
;;             ;; assume (first value) exists
;;             (slog :debug "Is a hash? ~A" (hash-table-p (href (first value) "range" "start")))
;;             ;; (slog :debug "Range start: line ~A char ~A" (href (first value) "range" "start" "line") (href (first value) "range" "start" "character"))
;;             (slog :debug " New text: ~A" (gethash "newText" (first value)))

;;             nil)))

;;;;;;; Below here was an initial version which began to use indentify for the entire file.
;; I abandoned this and copie din a ton of Alive LSP code thinking that I needed to handle incremental formatting
;; somehow, but after a closer look it appears Alive just formats the whole file. Unsure if it also does this on
;; incremental format requests. But yes, it does this even in "on-type". Strange... maybe can just format whole file
;; w/ indentify, instead?

;; (in-package :clef-lsp/document)

;; ;; TODO: Remove indentify as it cannot generate incremental text-edit objects needed by the LSP
;; ;; (indentify:load-default-templates)

(defun get-last-line-info (file-content)
       "Returns (line-number . last-char-position-on-line)
   line-number: the line number of the last line (1-indexed)
   last-char-position-on-line: the position of the last character on that line (1-indexed)"
       (let* ((length (length file-content))
              (last-newline-pos (position #\newline file-content :from-end t)))
             (let ((line-number (+ 1 (count #\newline file-content)))
                   (last-char-pos (if last-newline-pos
                                      (- length last-newline-pos 1)
                                      length)))
                  (values line-number last-char-pos))))

;; ;; TODO: Really not sure if indentify is OK to use long-term, but we definitely need to honor the various
;; ;; formatting options: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#formattingOptions
(defun handle-text-document-formatting (message)
       (let* ((params (clef-jsonrpc/types:request-params message))
              (file-uri (href params "text-document" "uri"))
              (document-text (gethash file-uri clef-lsp/server:*documents*))
              (pos (href params "position"))
              (options (href params "options")))
             ;; Format the entire file with cl-indentify
             (slog :debug "About to do formatting on ~A" file-uri)
             (let ((output (make-string-output-stream)))
                  (indentify:indentify
                    (make-string-input-stream document-text)
                    output)
                  (let* ((output-string (get-output-stream-string output)))
                        (multiple-value-bind (last-line last-char) (get-last-line-info document-text)
                                             (slog :debug "Did formatting on ~A" file-uri)
                                             (slog :debug "lastline: ~A last-char: ~A" last-line last-char)
                                             ;; Do NOT update the document here, as the editor will send us a didChange with the new edits
                                             ;; (setf (gethash file-uri clef-lsp/server:*documents*) output-string)
                                             (list (dict "range" (dict "start" (dict "line" 0 "character" 0)
                                                                       "end" (dict "line" last-line "character" last-char))
                                                         "newText" output-string)))))))
