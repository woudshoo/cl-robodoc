;;;; package.lisp

(defpackage #:cl-robodoc
  (:use #:cl
	#:alexandria
	#:split-sequence
	)
  (:export
   :read-directory-and-collect-robodoc-entries
   :read-file-and-split-to-robodoc
   :bag-classes
   :html-dirs-for-organized
   :source-dir-to-html-classes))

