;;;; cl-robodoc.asd

(asdf:defsystem #:cl-robodoc
  :serial t
  :depends-on (:alexandria
	       :split-sequence
	       :fset
	       :cl-fad
	       :cxml
	       :cl-ppcre
	       :flexi-streams
	       :trivial-project-pathname
	       :external-program)
  :components ((:file "package")

	       (:file "split-text-comment")
	       (:file "split-robodoc-sections")
	       (:file "beautify-section")
	       (:file "parse-source-2")
	       (:file "sax-handlers")
	       (:file "xml-source")))

