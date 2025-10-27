(in-package :clef-lsp/document)

(defun update-document-text (document-text start-line start-char end-line end-char new-text)
    "Replace text in document-text from (start-line, start-char) to (end-line, end-char) with new-text."
    (let ((start-offset (line-char-to-offset document-text start-line start-char))
          (end-offset (line-char-to-offset document-text end-line end-char)))
        (slog :debug "offsets: ~A to ~A~%" start-offset end-offset)
        (concatenate 'string
            (subseq document-text 0 start-offset)
            new-text
            (subseq document-text end-offset))))

(defun find-nth-newline (string n)
    "Find the position of the nth newline in the string."
    (loop with pos = -1
          repeat n
          do (setf pos (position #\Newline string :start (1+ pos)))
          while pos
          finally (return pos)))

(defun line-char-to-offset (string line char)
    (if (= line 0)
        char
        (let ((newline-pos (find-nth-newline string line)))
            (if newline-pos
                (+ newline-pos 1 char) ; +1 to move past the newline
                (length string))))) ; fallback: end of string


(defun handle-text-document-did-change (message)
    ;; Unpack the params into the document uri and range/text data
    (let* ((params (clef-jsonrpc/types:request-params message))
           (document-uri (href params "text-document" "uri"))
           (content-changes (href params "content-changes")))
        (slog :debug "Handling didChange for document: ~A" document-uri)
        ;; Loop over each content-change and print its diff vars for debugging
        (dotimes (i (length content-changes))
            (let ((content-change (aref content-changes i)))
                (slog :debug "Content-change is: ~A" content-change)
                (let* ((document (gethash document-uri clef-lsp/server:*documents*))
                       (range-start-line (href content-change "range" "start" "line"))
                       (range-start-char (href content-change "range" "start" "character"))
                       (range-end-line (href content-change "range" "end" "line"))
                       (range-end-char (href content-change "range" "end" "character"))
                       (new-text (href content-change "text")))
                    ;; (slog :debug "document before change: ~A~%" document)
                    ;; (slog :debug "content-change: ~A~%" content-change)
                    ;; (slog :debug "line ~A char ~A to line ~A char ~A with text: ~A~%"
                    ;;       range-start-line range-start-char range-end-line range-end-char new-text)
                    ;; Update the relevant document
                    (let ((updated-document-text (update-document-text document range-start-line range-start-char
                                                                       range-end-line range-end-char new-text)))
                        (setf (gethash document-uri clef-lsp/server:*documents*) updated-document-text))))))

    nil)
