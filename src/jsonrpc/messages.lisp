(in-package :clef-jsonrpc/messages)

(defun read-header-lines (stream)
    "Read header lines from a binary stream until an empty line is found."
    (let ((lines '())
          (line-bytes '()))
        (loop
     for byte = (read-byte stream nil)
     while byte
     do (cond
         ((= byte #.(char-code #\Newline))
             (let* ((line (babel:octets-to-string (coerce (nreverse line-bytes) '(vector (unsigned-byte 8))) :encoding :utf-8)))
                 (setf line-bytes '())
                 (if (string= (string-trim '(#\Return #\Newline) line) "")
                     (return (nreverse lines))
                     (push line lines))))
         ((= byte #.(char-code #\Return))
             ;; ignore, wait for newline
            )
         (t (push byte line-bytes))))))

(defun read-lsp-message (stream)
    "Read an LSP message from a binary stream according to LSP spec."
    (let ((headers (make-hash-table :test 'equal))
          (content-length nil))
        ;; Read headers
        (dolist (line (read-header-lines stream))
            (let* ((sep (position #\: line))
                   (key (and sep (string-downcase (subseq line 0 sep))))
                   (value (and sep (string-trim '(#\Space) (subseq line (1+ sep))))))
                (when (and key value)
                      (setf (gethash key headers) value)
                      (when (string= key "content-length")
                            (setf content-length (parse-integer value :junk-allowed t))))))
        (unless content-length
            (error "Missing Content-Length header in LSP message"))
        (slog :debug "Read content-length header: ~A" content-length)
        ;; Read content
        (let* ((buffer (make-array content-length :element-type '(unsigned-byte 8)))
               (nread (read-sequence buffer stream)))
            (unless (= nread content-length)
                (error "Failed to read full LSP message body"))
            (let* ((message-hash (com.inuoe.jzon:parse (babel:octets-to-string buffer :encoding :utf-8)))
                   (message (make-instance 'jsonrpc-request
                                :id (gethash "id" message-hash)
                                :method (gethash "method" message-hash)
                                :params (gethash "params" message-hash))))
                ;; (slog :debug ">>>>>>> Read LSP message-hash: ~A" message-hash)
                ;; (format t "Keys: ~A~%" (loop for k being the hash-keys of message-hash collect k))
                ;; (format t "gethash :params: ~A~%" (gethash :params message-hash))
                ;; (format t "gethash \"params\": ~A~%" (gethash "params" message-hash))
                ;; (slog :debug ">>>>>>> Read LSP message-hash: ~A" message-hash)
                ;; (slog :debug ">>>>>>> Read LSP params: ~A" (gethash "params" message-hash))
                message))))

(defun write-lsp-message (response stream)
    "Write an LSP message to a binary stream according to LSP spec."
    ;; Response is either a jsonrpc-response or a jsonrpc-error
    (let* ((json (com.inuoe.jzon:stringify response :stream nil))
           (bytes (babel:string-to-octets json :encoding :utf-8))
           (length (length bytes))
           (header (format nil "Content-Length: ~D~C~C~C~C"
                       length #\Return #\Newline #\Return #\Newline))
           (header-bytes (babel:string-to-octets header :encoding :utf-8)))
        (slog :debug "Writing LSP message with content-length: ~A" length)
        (slog :debug "LSP message content: ~A" json)
        (write-sequence header-bytes stream)
        (write-sequence bytes stream)
        (force-output stream)))
