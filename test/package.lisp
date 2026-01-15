(defpackage :clef-test
  (:use :cl)
  (:import-from :serapeum :dict :href)
  (:export :run-all-tests
           :*test-results*
           :deftest
           :assert-equal
           :assert-true
           :assert-not-nil
           :assert-nil
           :make-lsp-request
           :send-request
           :with-test-server))
