
(asdf:defsystem #:cl-robodoc-test
  :serial t
  :depends-on (:lift :cl-robodoc)
  :components ((:file "test-package")
	       (:file "beautify-section-test")))

