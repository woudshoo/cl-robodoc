(in-package #:cl-robodoc)

;;; Design
;;;
;;;   The generic design is (logically) as follows:
;;;
;;;   1 - read a file and split into robodoc blocks.  A robodoc block is either source or comment.
;;;   2 - split the stream of block into sections.  (indicated by **** comment starters)
;;;   3 - Organize the sections, e.g. collect all methods together with the class into `class-description' instances.
;;;   4 - when exporting, interprete the sections. e.g. make headings, interprete +uml sections, gnuplot sections etc.
;;;   5 - Export to HMTL files and create an index.html
;;;
;;;   Now implementation wise, step 1 creates a stream of blocks which is split by step 2.  The result is a list.
;;;   Step 3 creates a map from 'class' name to a `class-description'
;;;   Step 4 & 5 are a bit mixed.  The parsing into headings, lists etc is done first.  This data is 
;;;   put into a sax-handler.  Certain sax-handlers will take custom tags, such as :uml or :plot and
;;;   transform transform the result to valid html/xml.
;;;   
;;;
(project-pathname:define project-path (:asdf "cl-robodoc")
  (:resources "resources/"))


(defun read-file-and-split-to-robodoc (file-name)
  "Reads the FILE-NAME and parses the content into robodoc sections.  
See documentation of ROBODOC-SPLITTER for the resulting output."
  (with-open-file (s file-name :external-format :utf-8)
    (let ((splitter (make-instance 'robodoc-splitter
				    :section-splitter (make-instance 'section-splitter
								     :in-stream s ))))
      (loop :for block = (next splitter) :while block :collect block))))

(defun read-directory-and-collect-robodoc-entries (directory)
  "Reads all H and CPP file in the DIRECTORY and collected the ROBODOC-SPLITTER sections."
  (let ((result (list)))
    (cl-fad:walk-directory directory 
			   (lambda (file)
			     (setf result 
				   (concatenate 'list result 
						(read-file-and-split-to-robodoc file))))
			   :directories nil
			   :test (lambda (name)
				   (member (pathname-type name) '("H" "CPP") :test #'string-equal)))
    result))



(defun html-for-entries (entry-list &key class (header-p t) sink)
  "Call `html-for-entry' on all entries in `entry-list'.  However, the entry-list is sorted on `simple-name'.
See docuemntation for `html-for-entry'."
  (loop :for entry :in (sort (copy-seq entry-list) #'string< :key #'simple-name) 
     :do
     (html-for-entry entry :class class :header-p header-p :sink sink)))


(defun html-for-entry (entry &key class (header-p t) (sink))
  "Writes to a sax `sink' the formated entry.
* If header-p is true, a :h2 node is written with as content the simple-name for the entry.
* If class is specified,  the body of the entry is wrapped in a :div element with class attribute equal to `class'.

See documentation on `beautify-section' how the entry is actually formatted."
  (when header-p  (map-node sink `(:h2 ,(simple-name entry)) nil))

  (when class
    (sax:start-element sink nil nil "div"
		       (list (sax:make-attribute :qname "class" :value class))))
  (mapcar #'(lambda (ts) 
	      (mapcar #'(lambda (s) (map-node sink s nil)) ts))
	  (mapcar #'beautify-section (nth 2 entry)))

  (when class
    (sax:end-element sink nil nil "div")))



(defun write-html-header (name)
  "Writes the default header for all files.  It will write to the default sax stream.
This will create the :head and :script element."
  (cxml:with-element "head"
    (cxml:with-element "title" (cxml:text name))
    (cxml:with-element "link"
      (cxml:attribute "href" "doc.css")
      (cxml:attribute "rel" "stylesheet")
      (cxml:attribute "type" "text/css"))
    (cxml:with-element "meta"
      (cxml:attribute "http-equiv" "content-type")
      (cxml:attribute "content" "application/xhtml+xml")
      (cxml:attribute "charset" "UTF-8"))
  (cxml:with-element "script"
      (cxml:attribute "type" "text/javascript")
      (cxml:attribute "src" "MathJax.js?config=TeX-MML-AM_HTMLorMML"))))



(defun html-for-chapter (org chapter base-dir &key (active-words))

  (with-open-file (out (html-file-name base-dir chapter)
		       :direction :output
		       :external-format :utf-8
		       :if-exists :supersede)
    (write-string "<!DOCTYPE html>" out)
    (cxml:with-xml-output 
	(make-instance 'uml-transcribe-handler :handlers 
		       (list 
			(if active-words
			    (make-instance 'linkify-handler 
					   :words active-words 
					   :exclude-words (fset:set chapter)
					   :handlers (list 
						      (cxml:make-character-stream-sink out 
										       :canonical t
										       :indentation nil)))
			    (cxml:make-character-stream-sink out 
							     :canonical t
							     :indentation nil))))
      (cxml:with-element "html"
	(write-html-header chapter)
	(cxml:with-element "body"
	  (cxml:with-element "h1" (cxml:text chapter))
	  (cxml:with-output-sink (sink)
	    (let ((value (fset:lookup org chapter)))
	      (html-for-entry (class-description-class-entry value) :header-p nil :class "CLASS" :sink sink)

	      (html-for-entries (class-description-property-entries value) :class "PROPERTY" :sink sink)
	      (html-for-entries (class-description-method-entries value) :class "METHOD" :sink sink)
	      (html-for-entries (class-description-function-entries value) :class "FUNCTION" :sink sink))))))))



(defun keys-from-map (org)
  "Returns as a list all keys of ORG (which should be an fset:map)"
  (let ((result))
    (fset:do-map (key value org)
      (declare (ignore value))
      (push key result))
    result))

(defun write-html-index (org base-dir)
  "Write the index.html file based upon the entries in `org' to `base-dir'."
  (with-open-file (out (html-file-name base-dir "index")
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create
		       :external-format :utf-8)
    (cxml:with-xml-output (cxml:make-character-stream-sink out :canonical t :indentation nil)
      (cxml:doctype "html" nil nil)
      (cxml:with-element "html"
	(write-html-header "Index")
	(cxml:with-element "body"
	  (cxml:with-element "h1" (cxml:text "Index"))
	  (cxml:with-element "ul"
	    (loop :for key :in (sort (keys-from-map org) #'string<)
	       :do
	       (cxml:with-element "li"
		 (cxml:with-element "a"
		   (cxml:attribute "href" (format nil "~A.html" key))
		   (cxml:text key))))))))))
	


(defun write-additional-files (directory)
  "Write supporting files for the documentation to `directory'.
The supporting files are CSS files, and supporting javascript files."
  (ensure-directories-exist directory)
  (cl-fad:copy-file (project-path "doc.css" :resources)
		    (merge-pathnames "doc.css"   directory)
		    :overwrite t)
  (cl-fad:copy-file (project-path "MathJax.js" :resources) 
		    (merge-pathnames "MathJax.js" directory)
		    :overwrite t)
  (copy-directory-recursively (project-path "extensions/" :resources)
			      (merge-pathnames "extensions/" directory))
  (copy-directory-recursively (project-path "images/" :resources)
			      (merge-pathnames "images/" directory))
  (copy-directory-recursively (project-path "config/" :resources)
			      (merge-pathnames "config/" directory))
  (copy-directory-recursively (project-path "jax/" :resources)
			      (merge-pathnames "jax/" directory))
  (copy-directory-recursively (project-path "fonts/" :resources)
			      (merge-pathnames "fonts/" directory)))

(defun html-dirs-for-organized (org directory &optional &key (copy-support-files t))
  "Given a map of class-description objects in `org', write
the complete html structure in target directory `directory'.
This will write the index file, copy the mathjax javascript, write
all the individual html files for the entries in `org' etc."
  (when copy-support-files (write-additional-files directory))
  (write-html-index org directory)
  (fset:do-map (key value org)
    (declare (ignore value))
    (html-for-chapter org key directory :active-words org)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct class-description 
  "Cotnains a group of robodoc entries, separated by the type of entry."
  class-name
  class-entry
  method-entries
  property-entries
  function-entries)


(defun bag-classes-and-functions (robodoc-entries)
  "Takes a list of parsed `robodoc-entries' and 
organizes them class-description entries.  
The class-description entries created are returned as a map, keyed
on the class-name."
  (let ((result (fset:map)))
    (loop :for entry :in robodoc-entries :do
       (when-let (class (car (classification entry)))
	 (let ((cd (or (fset:lookup result (name-of-group entry))
			  (make-class-description :class-name (name-of-group entry)))))
	   (case class
	     (#\c (setf (class-description-class-entry cd) entry))
	     (#\m (push entry (class-description-method-entries cd)))
	     (#\p (push entry (class-description-property-entries cd)))
	     (#\f (push entry (class-description-function-entries cd))))
	   (setf result (fset:with result (name-of-group entry) cd)))))
    result))


(defun source-dir-to-html-classes (sources-dir target-dir 
				   &optional &key (copy-support-files t))
  "Convert the source code for files found in sources-dir to html
which will be exported to target-dir."
  (html-dirs-for-organized 
   (bag-classes-and-functions
    (read-directory-and-collect-robodoc-entries sources-dir))
   target-dir :copy-support-files copy-support-files))



(defun main (argv)
  (format t "Arguments are: ~S~%" argv)
  (format t "Project Path: ~A~%" (project-path "doc.css" :resources))
  (source-dir-to-html-classes  (second argv) 
			       (third argv)))
