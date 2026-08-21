(defsystem :clef-runner
  :description "A runner that gives a Common Lisp program process-level guarantees"
  :author "Nathan Weir"
  :license "MIT"
  :version "0.0.1"
  :depends-on ("clef-conditions" "uiop")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "runtime")
                             (:file "compile")
                             (:file "cli")))))

(defsystem :clef-runner/test
  :description "Tests for clef-runner"
  :author "Nathan Weir"
  :license "MIT"
  :depends-on ("clef-runner")
  :serial t
  :components ((:module "test"
                :components ((:file "package")
                             (:file "runtime-tests")
                             (:file "cli-tests")))))
