(defpackage :clef-conditions
  (:use :cl)
  (:export
   ;; the structure
   #:diagnostic
   #:diagnostic-p
   ;; Constructing one by hand is how clef-lint feeds its findings through
   ;; this renderer, so lint output and runtime diagnostics read as one tool.
   #:make-diagnostic
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
   #:*context-lines*
   ;; Extension point: kinds whose FILE-POSITION is the exact answer, so the
   ;; renderer draws a caret without the "location is approximate" note.
   ;; Exported for clef-lint, whose positions come from a parse tree and are
   ;; always exact.
   #:*exactly-located-kinds*))
