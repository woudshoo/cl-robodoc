(in-package :cl-robodoc-test)

(defun run-all-tests ()
  (mapcar #'(lambda (s) (run-tests :suite s))
	  '(pre-element-2 beautify-simple beautify-lists-1 p-element-1 split-robodoc-tests)))
