(defpackage :clef-runner
  (:use :cl)
  (:export
   ;; exit-code contract
   #:+exit-success+
   #:+exit-failure+
   #:+exit-usage+
   #:+exit-diagnostics+
   ;; the profile
   #:call-with-runtime
   #:with-runtime
   #:*optimize-policy*
   #:*warnings-as-errors*
   #:*min-severity*
   #:*diagnostic-stream*
   ;; running things under it
   #:collect-diagnostics
   #:report-diagnostics
   #:run-file
   #:run-system
   ;; command line
   #:main
   #:parse-args
   #:*usage*))
