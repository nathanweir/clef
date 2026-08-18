(defsystem :clef-conditions
  :description "Structured extraction and humane rendering of Common Lisp conditions"
  :author "Nathan Weir"
  :license "MIT"
  :version "0.0.1"
  :depends-on ()
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "extract")
                             (:file "render")))))

(defsystem :clef-conditions/test
  :description "Tests for clef-conditions"
  :author "Nathan Weir"
  :license "MIT"
  :depends-on ("clef-conditions")
  :serial t
  :components ((:module "test"
                :components ((:file "package")
                             (:file "extract-tests")
                             (:file "render-tests")))))
