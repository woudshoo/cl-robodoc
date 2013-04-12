(in-package #:cl-robodoc)

(defun parse-namestring-as-directory (string)
  "Returns a pathname assuming the string indentifies a directory.
This is basically the same as calling `pathname', 
however it will do some magic if the argument is something like:

   \"/tmp/A\"
or 
   \"/tmp/A.b\"

Because the default implementation assumes that A or A.b are a file name or
a file and type."
  (let* ((namestring (pathname string))
	 (name (pathname-name namestring))
	 (type (pathname-type namestring)))
    (if (or name type)
	(merge-pathnames
	 (make-pathname :directory `(:relative ,(format nil "~{~A~}~{.~A~}" 
							(ensure-list name)
							(ensure-list type))))
	 (make-pathname :name nil :type nil :defaults namestring ))
	namestring)))

(defun html-file-name (dir name)
  "Returns a pathname representing a file with name NAME in directory DIR with type HTML."
  (merge-pathnames (make-pathname :name name :type "html") dir))


(defun map-name (name source target)
  "If `name' is a pathname which is located in a source directory `source', create a pathname in the `target' directory.
E.g.:
name = /a/b/c/d/e.f, source = /a/b, target = /x/y/z  ===>  /x/y/z/c/d/e.f "
  (let* ((source-name (merge-pathnames name source))
	 (source-name-type (make-pathname :name (pathname-name source-name)
					  :type (pathname-type source-name)))
	 (source-name-dirs (pathname-directory source-name))
	 (source-dirs (pathname-directory source))
	 (start-point (search source-dirs source-name-dirs :test #'equalp :from-end t))
	 (new-dirs (subseq source-name-dirs (+ start-point (length source-dirs)))))
    (merge-pathnames source-name-type (merge-pathnames (make-pathname :directory (cons :relative new-dirs)) target))))



(defun copy-directory-recursively (source-dir target-dir)
  "Utility function which copies files recursively from `source-dir' to `target-dir'.
Missing directories are created."
  (cl-fad:walk-directory source-dir 
			 (lambda (fn)
			   (when (pathname-name fn)
			     (let ((target-name (map-name fn source-dir target-dir)))
			       (ensure-directories-exist target-name)
			       (cl-fad:copy-file fn target-name :overwrite t))))
			 :directories :breadth-first))
