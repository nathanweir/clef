(in-package :clef-log)

;; TODO: Actually observe this
(defparameter +log-level+ :debug
              "The current log level. Possible values are :debug, :warn, :error, and :info.")
(defparameter *log-levels*
              '(:debug 0
                       :warn 1
                       :error 2
                       :info 3)
              "Mapping of log levels to their severity.")

(defparameter +log-mode+ :console
              "The current log mode. Possible values are :file and :console.")

(defparameter +log-file-path+ #p"/home/nathan/clef.log"
              "The file path for log output when in file mode.")

(defun formatted-current-time ()
    "Returns the current time formatted as a string."
    (multiple-value-bind (sec min hour) (decode-universal-time (get-universal-time))
        (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))

(defun init (log-mode)
    "Initializes the logging system."
    (setf +log-mode+ log-mode)
    (when (eq +log-mode+ :file)
          (with-open-file (stream +log-file-path+ :direction :output :if-does-not-exist :create :if-exists :supersede)
              (format stream "[~A] [INFO] Log initialized.~%" (formatted-current-time)))))

;; Short for "s"erver log. Probably worth renaming
(defun slog (level message &rest args)
    "Logs a MESSAGE at the given LEVEL with optional ARGS for formatting."
    (let* ((level-severity (getf *log-levels* level))
           (current-severity (getf *log-levels* +log-level+)))
        (when (<= current-severity level-severity)
              (let ((formatted-message (if args
                                           (apply #'format nil message args)
                                           message))
                    (timestamp (formatted-current-time)))
                  (cond
                   ((eq +log-mode+ :console)
                       (format t "[~A] [~A] ~A~%" timestamp level formatted-message))
                   ((eq +log-mode+ :file)
                       (with-open-file (stream +log-file-path+ :direction :output :if-does-not-exist :create :if-exists :append)
                           (format stream "[~A] [~A] ~A~%" timestamp level formatted-message)))
                   (t
                       (error "Unknown log mode: ~A" +log-mode+)))))))
