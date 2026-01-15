(defsystem :clef-test
  :description "Tests for CLEF - Common Lisp Editor Facilitator"
  :author "Nathan Weir"
  :license "MIT"
  :depends-on ("clef"
               "serapeum"
               "bordeaux-threads"
               "com.inuoe.jzon"
               "babel"
               "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "framework")
               (:file "lifecycle-tests")
               (:file "document-tests")
               (:file "diagnostic-tests")))
