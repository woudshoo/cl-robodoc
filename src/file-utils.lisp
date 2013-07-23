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
  (merge-pathnames (make-pathname :name name :type "html") 
		   (parse-namestring-as-directory dir)))


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
	 (ttt (assert start-point (source-dirs source-name-dirs) "SD: ~S SND: ~S" source-dirs source-name-dirs ))
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



(defun read-directory-as-tar-vector (directory)
  "Returns a array "
  (external-program:run "tar" `("-cf" "/tmp/ta1.tar" "-C" ,directory "."))
  (with-open-file (stream "/tmp/ta1.tar" :element-type '(unsigned-byte 8))
    (let ((tar-sequence (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence tar-sequence stream)
      tar-sequence)))

(defun extract-resources (source-tar source-directory target-directory)
  "Populates the `target-directory' with either the `source-tar' content
  or the `source-directory' content.

If specified, the `source-tar' should be a sequence containing the content of a tar file.
The content of the tar sequence will be extracted at the target directory.

If `source-tar' is nil the code will recursively copy the content of `source-directory' to 
the `target-directory'.

Use cases for this function are deployment of lisp executables.  It can read in the content 
of support files in a vector which will be saved in the image.  The run time code can 
than extract it when needed."
  (if source-tar
      (miniuntar:unpack-tar-sequence source-tar :directory target-directory)
      (copy-directory-recursively source-directory target-directory)))
