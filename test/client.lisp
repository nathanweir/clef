(ql:quickload :jsonrpc)
(ql:quickload :serapeum)

;; Run via `ros -- test/client.lisp`

(defvar *client* (jsonrpc:make-client))

;; TODO: Move this into the code-base proper, as some universal util
;; (or don't, it's pretty bad)
(defmethod print-object ((object hash-table) stream)
    (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
        (loop for key being the hash-keys of object
              using (hash-value value)
              collect (list key value))))

(defun main ()
    (jsonrpc:client-connect *client* :url "http://127.0.0.1:8666" :mode :tcp)
    (let ((resp (jsonrpc:call *client* "initialize" '(10 20))))
        (format t "Hello world ~A~%"
            (if (hash-table-p resp)
                (serapeum:pretty-print-hash-table resp)
                resp))))
