(defpackage :clef-lsp/src/lsp/document/formatting
  (:use :cl)
  (:import-from :indentify)
  (:import-from :clef-lsp/src/log #:slog)
  (:import-from :serapeum #:dict #:href)
  (:local-nicknames
    (:ctx :clef-lsp/src/context)
    (:rpc :clef-lsp/src/jsonrpc/types))
  (:export
   #:handle-text-document-formatting))

(in-package :clef-lsp/src/lsp/document/formatting)

;;;; textDocument/formatting.
;;;;
;;;; Whole-document formatting, delegated to cl-indentify. The endpoint with the
;;;; largest blast radius in the server: it returns a single edit replacing the
;;;; entire buffer, so anything wrong with either the range or the replacement
;;;; text damages the file rather than merely answering badly.
;;;;
;;;; TODO: cl-indentify only indents. It does not honour the FormattingOptions
;;;; the client sends -- tabSize, insertSpaces, trimTrailingWhitespace,
;;;; insertFinalNewline, trimFinalNewlines. See
;;;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#formattingOptions

(defun document-end-position (text)
  "The position just past the last character of TEXT, as (values line character).

Both 0-indexed, because LSP positions are.

This used to return a 1-INDEXED line, so the replace range always ended one line
past the end of the document -- measured on every case in
docs/experiments/lsp/11-formatting-contract.lisp. It appeared to work only
because clients clamp an out-of-range position back to the end of the document;
a client entitled to reject the range instead would have lost the edit."
  (let ((last-newline (position #\Newline text :from-end t)))
    (if last-newline
        (values (count #\Newline text)
                (- (length text) last-newline 1))
        (values 0 (length text)))))

(defun handle-text-document-formatting (message)
  "Handle a textDocument/formatting request."
  (let* ((params (rpc:request-params message))
         (file-uri (href params "text-document" "uri"))
         (document-text (gethash file-uri ctx:documents)))
    (slog :debug "About to do formatting on ~A" file-uri)
    (if (null document-text)
        #()
        ;; Answer with NO edits rather than a bad one if the formatter fails.
        ;;
        ;; INDENTIFY was called with nothing around it, and its output replaces
        ;; the whole buffer -- so a condition anywhere inside it turned into an
        ;; error response at best, and a buffer replaced with a partial write at
        ;; worst. Declining to format is always recoverable; the file is still
        ;; there.
        (let ((formatted
                (handler-case
                    (let ((output (make-string-output-stream)))
                      (indentify:indentify (make-string-input-stream document-text)
                                           output)
                      (get-output-stream-string output))
                  (error (e)
                    (slog :warn "[formatting] ~A left unformatted: ~A" file-uri e)
                    nil))))
          (cond
            ((null formatted) #())
            ;; Nothing to say beats saying "replace the file with itself",
            ;; which dirties the buffer and pushes an undo entry for no reason.
            ((string= formatted document-text) #())
            (t
             (multiple-value-bind (end-line end-character)
                 (document-end-position document-text)
               (slog :debug "[formatting] replacing through ~D:~D" end-line end-character)
               ;; A vector, not a list: this is a JSON array, and every other
               ;; handler in the server spells one as a vector.
               (vector
                (dict "range" (dict "start" (dict "line" 0 "character" 0)
                                    "end" (dict "line" end-line
                                                "character" end-character))
                      "newText" formatted)))))))))
