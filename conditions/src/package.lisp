(defpackage :clef-conditions
  (:use :cl)
  (:export
   ;; the structure
   #:diagnostic
   #:diagnostic-p
   #:diagnostic-severity
   #:diagnostic-kind
   #:diagnostic-symbol
   #:diagnostic-message
   #:diagnostic-file
   #:diagnostic-file-position
   #:diagnostic-source-path
   #:diagnostic-context
   #:diagnostic-source-form
   #:diagnostic-references
   ;; extraction
   #:extract
   #:condition-severity
   ;; rendering
   #:render
   #:render-to-string
   #:*color*
   #:*context-lines*))
