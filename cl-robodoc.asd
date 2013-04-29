;;;; cl-robodoc.asd

(asdf:defsystem #:cl-robodoc
  :name "cl-robodoc"
  :description "Source code documentation to HTML, inspired by robodoc"
  :version "0.1"
  :license "GPL"
  :author "Willem Rein Oudshoorn <woudshoo@xsall.nl>"
  :pathname "src/"
  :serial t
  :depends-on (:alexandria
	       :split-sequence
	       :fset
	       :cl-fad
	       :cxml
	       :cl-ppcre
	       :flexi-streams
	       :trivial-project-pathname
	       :colorize
	       :external-program
	       :miniuntar)
  :components ((:file "package")
	       (:file "split-text-comment")
	       (:file "split-robodoc-sections")
	       (:file "beautify-section")
	       (:file "file-utils")
	       (:file "parse-source-2")
	       (:file "sax-handlers")
	       (:file "lml-sax-util")))

