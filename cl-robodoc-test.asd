
(asdf:defsystem #:cl-robodoc-test
  :license "GPL"
  :pathname "test/"
  :serial t
  :depends-on (:lift :cl-robodoc)
  :components ((:file "test-package")
	       (:file "beautify-section-test")
	       (:file "split-robodoc-sections-test")
	       (:file "tests")))

