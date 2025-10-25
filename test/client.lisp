#-quicklisp
(let ((quicklisp-init
       (merge-pathnames "quicklisp/setup.lisp"
                        (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
          (load quicklisp-init)))


(ql:quickload :jsonrpc)
(ql:quickload :serapeum)
(ql:quickload :bordeaux-threads)
(require 'sb-posix)

(asdf:load-asd #P"/home/nathan/dev/clef/clef.asd")
(asdf:load-system :clef)

;; Run via `ros -- test/client.lisp`

(defvar *client* (jsonrpc:make-client))

;; TODO: Move this into the code-base proper, as some universal util
;; (or don't, it's pretty bad)
;; UPDATE: Almost certainly don't, can just use serapeum's pretty-print-hash-table instead:
;; https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#pretty-print-hash-table-ht-optional-stream
(defmethod print-object ((object hash-table) stream)
    (format stream "#HASH{~{~{(~a : ~a)~}~^ ~}}"
        (loop for key being the hash-keys of object
              using (hash-value value)
              collect (list key value))))

;; TODO: Try updating this to connect to the server in src/lsp/server
(defun main ()
    ;; Create two pipes: one for each direction
    (multiple-value-bind (c2s-read c2s-write) (sb-posix:pipe)
        (multiple-value-bind (s2c-read s2c-write) (sb-posix:pipe)
            ;; Server streams
            (let ((server-input (sb-sys:make-fd-stream c2s-read :input t :element-type '(unsigned-byte 8)))
                  (server-output (sb-sys:make-fd-stream s2c-write :output t :element-type '(unsigned-byte 8)))
                  ;; Client streams
                  (client-input (sb-sys:make-fd-stream s2c-read :input t :element-type 'character))
                  (client-output (sb-sys:make-fd-stream c2s-write :output t :element-type 'character)))
                ;; Start the server in a thread
                (bordeaux-threads:make-thread
                    (lambda ()
                        (clef-lsp/server:start :input server-input :output server-output)))
                ;; Connect the client
                (jsonrpc:client-connect *client*
                                        :mode :stdio
                                        :input client-input
                                        :output client-output))))
    (format t "Client connected to server.~%")
    (handler-case
            (let ((resp (jsonrpc:call *client* "initialize" '(10 20) :timeout 1.0)))
                (format t "Hello world ~A~%"
                    (if (hash-table-p resp)
                        (serapeum:pretty-print-hash-table resp)
                        resp)))
        (error (e)
            ;; Upon timeout this prints 'Timeout occurred while waiting for response: JSONRPC/BASE::JSONRPC-TIMEOUT does not designate a condition class.'
            ;; Not worth fixing right now
            (format t "Error occurred while waiting for response: ~A~%" e))))

(main)
