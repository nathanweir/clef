(in-package :clef-root)

;; claim-protocol-stream needs sb-posix at read time. Required here rather than
;; via an ASDF (:require ...) dependency: clef's own workspace-system loader
;; assumes every :depends-on entry is a symbol or string, so a list form there
;; breaks initialize for anyone whose project uses one.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defun env-log-settings ()
       "Logging config for the shipped binary, from the environment.

        Normal use writes nothing to disk. Set CLEF_LOG_FILE=/path/to/log to opt
        in when debugging -- no rebuild needed. A relative path is resolved
        against the process's working directory."
       (let ((path (sb-posix:getenv "CLEF_LOG_FILE")))
            (if (and path (plusp (length path)))
                (values :file (merge-pathnames path (uiop:getcwd)))
                (values :none nil))))

(defun start-server (&key (input *standard-input*) (output *standard-output*)
                          (log-mode :none) log-file-path)
       ;; Nothing may be logged before clef-lsp/server:start calls clef-log:init.
       (clef-lsp/server:start :input input :output output
                              :log-mode log-mode :log-file-path log-file-path))

(defun claim-protocol-stream ()
       "Move the real stdout to a private fd and point fd 1 at stderr.

        Returns a stream on the private fd; it is the only thing that may carry
        LSP framing. Everything else in the process -- stray format t, foreign
        library chatter, subprocesses that inherit fd 1 -- lands on stderr and
        cannot corrupt the client's stream."
       (let ((protocol-stream (sb-sys:make-fd-stream (sb-posix:dup 1)
                                                     :output t
                                                     :element-type :default
                                                     :buffering :full)))
            (sb-posix:dup2 2 1)
            protocol-stream))

(defun main ()
       "Toplevel entry point for the saved executable.

        The image already has every system loaded, so starting up touches no
        ASDF, no `make', no compiler and nothing from the dev shell. It also
        writes no log file unless CLEF_LOG_FILE asks for one."
       (sb-ext:disable-debugger)          ; never drop into a REPL on a pipe
       ;; Re-initialise ASDF against the environment we were actually launched
       ;; in; the image was dumped with its configuration cleared.
       (uiop:call-image-restore-hook)
       (let ((protocol-stream (claim-protocol-stream)))
            (handler-case
                (multiple-value-bind (log-mode log-file-path) (env-log-settings)
                  (start-server :output protocol-stream
                                :log-mode log-mode
                                :log-file-path log-file-path))
              (sb-sys:interactive-interrupt () (sb-ext:exit :code 0 :abort t))
              (error (e)
                     (format *error-output* "clef: fatal: ~A~%" e)
                     (finish-output *error-output*)
                     (sb-ext:exit :code 1 :abort t))))
       (sb-ext:exit :code 0))
