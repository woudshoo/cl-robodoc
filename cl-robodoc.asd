;;;; cl-robodoc.asd

(asdf:defsystem #:cl-robodoc
  :serial t
  :depends-on (:meta-sexp :alexandria :split-sequence :fset :cl-who :cl-fad :lml2 :cxml :cl-ppcre :flexi-streams)
  :components ((:file "package")

	       (:file "split-text-comment")
	       (:file "split-robodoc-sections")
	       (:file "beautify-section")
	       (:file "parse-source-2")
	       (:file "sax-handlers")
	       (:file "xml-source")
#|	       (:file "parse-source")
	       (:file "transform")
               (:file "cl-robodoc")
|#
))

