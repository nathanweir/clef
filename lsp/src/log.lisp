(in-package :clef-log)

;; Log level configuration (mutable, so use * convention)
(defvar *log-level* :debug
  "The current log level. Possible values are :debug, :info, :warn, and :error.")

(defparameter *log-levels*
              '(:debug 0
                :info 1
                :warn 2
                :error 3)
              "Mapping of log levels to their severity.")

(defvar *log-mode* :none
  "The current log mode: :none, :file or :console.

   Defaults to :none so that merely loading clef never writes to disk and never
   touches *standard-output* -- which is the LSP protocol stream. Callers opt in
   explicitly via INIT.")

(defvar *log-file-path* nil
  "The file path for log output when in :file mode. NIL means no file is known,
   in which case :file mode degrades to :none.")

(defun formatted-current-time ()
    "Returns the current time formatted as a string."
    (multiple-value-bind (sec min hour) (decode-universal-time (get-universal-time))
        (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))

(defun disable-logging (reason)
    "Turn logging off permanently. Logging is a diagnostic aid; never let it take
     the server down."
    (setf *log-mode* :none)
    (ignore-errors
      (format *error-output* "clef: logging disabled: ~A~%" reason)
      (finish-output *error-output*)))

(defun init (log-mode &key file-path)
    "Initializes the logging system.

     LOG-MODE is :none, :file or :console. In :file mode FILE-PATH (or an
     already-set *log-file-path*) is truncated. If the file cannot be opened --
     read-only filesystem, missing directory, no permission -- logging is
     switched off rather than signalling, so an unwritable log location can
     never prevent the server from starting."
    (when file-path
          (setf *log-file-path* file-path))
    (setf *log-mode* log-mode)
    (when (eq *log-mode* :file)
          (if (null *log-file-path*)
              (disable-logging "no log file path was configured")
              (handler-case
                (progn
                 (ensure-directories-exist *log-file-path*)
                 (with-open-file (stream *log-file-path*
                                         :direction :output
                                         :if-does-not-exist :create
                                         :if-exists :supersede)
                     (format stream "[~A] [INFO] Log initialized.~%"
                             (formatted-current-time))))
                (error (e)
                       (disable-logging (format nil "cannot write ~A (~A)"
                                                *log-file-path* e)))))))

;; Short for "s"erver log. Probably worth renaming
(defun slog (level message &rest args)
    "Logs a MESSAGE at the given LEVEL with optional ARGS for formatting."
    (let ((level-severity (getf *log-levels* level))
          (current-severity (getf *log-levels* *log-level*)))
        (when (and level-severity
                   current-severity
                   (<= current-severity level-severity)
                   (not (eq *log-mode* :none)))
              (let ((formatted-message (if args
                                           (apply #'format nil message args)
                                           message))
                    (timestamp (formatted-current-time)))
                  (case *log-mode*
                    ;; NB: :console writes to *standard-output*. Under the LSP
                    ;; server that stream is redirected to stderr, but never
                    ;; select :console for a process whose stdout is the client.
                    (:console
                     (ignore-errors
                       (format t "[~A] [~A] ~A~%" timestamp level formatted-message)))
                    (:file
                     (handler-case
                       (with-open-file (stream *log-file-path*
                                               :direction :output
                                               :if-does-not-exist :create
                                               :if-exists :append)
                           (format stream "[~A] [~A] ~A~%"
                                   timestamp level formatted-message))
                       (error (e)
                              (disable-logging
                                (format nil "write to ~A failed (~A)"
                                        *log-file-path* e)))))
                    ;; Unknown mode: stay quiet rather than signalling from a
                    ;; logging call buried in a request handler.
                    (t (disable-logging (format nil "unknown log mode ~S" *log-mode*))))))))
