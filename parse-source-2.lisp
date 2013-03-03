(in-package #:cl-robodoc)

(project-pathname:define project-path (:asdf "cl-robodoc")
  (:resources "resources/"))



(defun test-ss (&optional (file-name "d:/Sources/StorageMagic/source/CppSource/DSSItem.cpp"))
  (let ((s (open file-name)))
    (cons s
	  (make-instance 'section-splitter 
			 :in-stream s))))
(defun test-sss (&optional (file-name "d:/Sources/StorageMagic/source/CppSource/DSSItem.cpp"))
  (let ((s (open file-name)))
    (cons s
	  (make-instance 'robodoc-splitter
			 :section-splitter (make-instance 'section-splitter 
							  :in-stream s)))))


(defun read-file-and-split-to-robodoc (file-name)
  "Reads the FILE-NAME and parses the content into robodoc sections.  
See documentation of ROBODOC-SPLITTER for the resulting output."
  (with-open-file (s file-name :external-format :utf-8)
    (let ((splitter (make-instance 'robodoc-splitter
				    :section-splitter (make-instance 'section-splitter
								     :in-stream s ))))
      (loop :for block = (next splitter) :while block :collect block))))

(defun read-directory-and-collect-robodoc-entries (&optional (directory "d:/Sources/StorageMagic/source/CppSource/"))
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

(defun organize (all)
  "Returns a nested structure of 'bag/set' of the entries in all"
  (loop 
     :for entry :in all
     :for keys = (rest (second entry))
     :for result = (fset:map ((car keys) (list entry))) 
     :then (fset:with result (car keys) (cons entry (fset:lookup result (car keys))))
     :finally (return result)))


(defun html-for-entries (entry-list &key class (header-p t) sink)
  (loop :for entry :in (sort (copy-seq entry-list) #'string< :key #'simple-name) 
     :do
     (html-for-entry entry :class class :header-p header-p :sink sink)))


(defun html-for-entry (entry &key class (header-p t) (sink))
  (when header-p  (map-node sink `(:h2 ,(simple-name entry)) nil))

  (when class
    (sax:start-element sink nil nil "div"
		       (list (sax:make-attribute :qname "class" :value class))))
  (mapcar #'(lambda (ts) 
	      (mapcar #'(lambda (s) (map-node sink s nil)) ts))
	  (mapcar #'beautify-section (nth 2 entry)))

#+nil  (sax:characters sink "
")
  (when class
    (sax:end-element sink nil nil "div")))




(defun html-file-name (dir name)
  (merge-pathnames (make-pathname :name name :type "html") dir))

;
; <meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\">





(defun write-html-header-for-help (name &key (is-parent nil))
  (flet ((element (key value)
	   (cxml:with-element "meta"
	     (cxml:attribute "name" key)
	     (cxml:attribute "content" value))))
    (cxml:with-element "head"
      (cxml:with-element "title" (cxml:text name))
      (element "Microsoft.Help.Id" (if is-parent "StorageMagicApp-1"
					      (format nil "StorageMagicApp-1-~A" name)))
      (element "Microsoft.Help.Locale" "en-us")
      (element "Microsoft.Help.TopicLocale" "en-us")
      (element "Microsoft.Help.TopicVersion" "10")
      (element "Microsoft.Help.SelfBranded" "false")
      (element "Microsoft.Help.TocParent" (if is-parent "-1" "StorageMagicApp-1"))
      (element "Microsoft.Help.TocOrder" "0")
      (element "Microsoft.Help.Keywords" (format nil "StorageMagic ~A" name))
      (element "Microsoft.Help.F1" name)
      (element "Description" (format nil "Description of ~A" name))
      (cxml:with-element "link"
	(cxml:attribute "href" "doc.css")
	(cxml:attribute "rel" "stylesheet")
	(cxml:attribute "type" "text/css")))))



(defun write-html-header (name)
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
#+nil    (cxml:with-element "script"
      (cxml:attribute "type" "text/javascript")
      (cxml:attribute "src" "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_HTMLorMML"))
  (cxml:with-element "script"
      (cxml:attribute "type" "text/javascript")
      (cxml:attribute "src" "MathJax.js?config=TeX-MML-AM_HTMLorMML"))))

(defun write-html-footer (name stream)
  (format stream "</body></html>"))



(defun write-lines-to-file (base-name extension lines)
  (let ((name (merge-pathnames (make-pathname :type extension) base-name)))
    (with-open-file (out name
			 :direction :output
			 :if-exists :overwrite
			 :if-does-not-exist :create)
      (loop :for line :in lines :do
	 (write-string line out)))
    name))

#+nil (defun process-uml-to-image (args body stream)
  "This is called for a form like:
 (:uml uml diagram text) or ((:uml . args) uml diagram text)

The args are a property list and the body is a list containing the body of the the
uml diagram.
They body should be a list of strings which are being fed into the plantuml.jar to creaate an image.

As a side effect, this function will write to STREAM a piece of html which will trigger the browser to render
the resulting uml image.   For this there are two strategies:

1 - Write the uml image to a separate file e.g. /tmp/xxx.png, and write to the stream
  <img src=/tmp/xxx.png>  

2 - Includde the images binary data, e.g.:  <img src=data:image/png;base64,.... >

"
  (write-string "<img src=\"data:image/png;base64," stream)
  (let ((tmp-name (parse-namestring (format nil "d:/tmp/~A" (random (expt 10 10))))))
    
    (sb-ext:run-program "java" `("-jar" "d:/Tools/bin/plantuml.jar" 
				 ,(namestring (write-lines-to-file tmp-name "uml" body)))
		 :search t)
    (with-open-file (in (merge-pathnames (make-pathname :type "png") tmp-name)
			:direction :input
			:element-type '(unsigned-byte 8))
      (s-base64:encode-base64 in stream)))
  (write-string "\">" stream))

#+nil (defun process-uml-to-image (args body stream)
  "This is called for a form like:
 (:uml uml diagram text) or ((:uml . args) uml diagram text)

The args are a property list and the body is a list containing the body of the the
uml diagram.
They body should be a list of strings which are being fed into the plantuml.jar to creaate an image.

As a side effect, this function will write to STREAM a piece of html which will trigger the browser to render
the resulting uml image.   For this there are two strategies:

1 - Write the uml image to a separate file e.g. /tmp/xxx.png, and write to the stream
  <img src=/tmp/xxx.png>  

2 - Includde the images binary data, e.g.:  <img src=data:image/png;base64,.... >

"
;  (write-string "<img src=\"data:image/png;base64," stream)
  (let ((tmp-name (parse-namestring (format nil "d:/tmp/~A" (random (expt 10 10))))))
    
    (sb-ext:run-program "java" `("-jar" "d:/Tools/bin/plantuml.jar" "-tsvg" 
				 ,(namestring (write-lines-to-file tmp-name "uml" body)))
		 :search t)
    (with-open-file (in (merge-pathnames (make-pathname :type "svg") tmp-name)
			:direction :input :element-type '(unsigned-byte 8))
;      (copy-stream in stream)
      (linkify in stream))
    (delete-file (merge-pathnames (make-pathname :type "uml") tmp-name))
    (delete-file (merge-pathnames (make-pathname :type "svg") tmp-name))))


;;; Note that the first form will NOT WORK!!
;;; The macro expansion will complain abot
;;; function calls because that is how it expands.
;;; But I am too lazy to make it work right now.
#+nil (lml2::def-special-html :uml
    #'(lambda (ent args argsp body)
	(declare (ignore ent))
	(unless argsp (error "Need body for uml sectoin"))
	`(progn
	   (process-uml-to-image ,args ,body lml2::*html-stream*)))
  
  #'(lambda (ent cmd args form subst unknown stream)
      (declare (ignore ent cmd subst unknown))
      (process-uml-to-image args (cdr form) stream)))


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
			    (make-instance 'linkify-handler :words active-words 
					   :exclude-words (fset:set chapter)
					   :handlers (list (cxml:make-character-stream-sink out 
											    :canonical t
											    :indentation nil)))
			    (cxml:make-character-stream-sink out :canonical t
							     :indentation nil))))
;      (cxml:doctype "html" nil nil) ;; doesn't do anything when canonical
      (cxml:with-element "html"
	(write-html-header chapter)
	(cxml:with-element "body"
	  (cxml:with-element "h1" (cxml:text chapter))
	  (cxml:with-output-sink (sink)
	    (let ((value (fset:lookup org chapter)))
	      (html-for-entry (class-description-class-entry value) :header-p nil :class "CLASS" :sink sink)

	      (html-for-entries  (class-description-property-entries value) :class "PROPERTY" :sink sink)
	      (html-for-entries  (class-description-method-entries value) :class "METHOD" :sink sink)
	      (html-for-entries  (class-description-function-entries value) :class "FUNCTION" :sink sink))))))))



(defun keys-from-map (org)
  (let ((result))
    (fset:do-map (key value org)
      (declare (ignore value))
      (push key result))
    result))

(defun write-html-index (org base-dir)
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
	


(defun map-name (name source target)
  (let* ((source-name (merge-pathnames name source))
	 (source-name-type (make-pathname :name (pathname-name source-name)
					  :type (pathname-type source-name)))
	 (source-name-dirs (pathname-directory source-name))
	 (source-dirs (pathname-directory source))
	 (start-point (search source-dirs source-name-dirs :test #'equalp :from-end t))
	 (new-dirs (subseq source-name-dirs (+ start-point (length source-dirs)))))
    (merge-pathnames source-name-type (merge-pathnames (make-pathname :directory (cons :relative new-dirs)) target))))

(defun copy-directory-recursively (source-dir target-dir)
  (cl-fad:walk-directory source-dir 
			 (lambda (fn)
			   (when (pathname-name fn)
			     (let ((target-name (map-name fn source-dir target-dir)))
			       (ensure-directories-exist target-name)
			       (cl-fad:copy-file fn target-name :overwrite t))))
			 :directories :breadth-first))

(defun write-additional-files (directory)
  (cl-fad:copy-file (project-path "doc.css" :resources)
		    (merge-pathnames "doc.css"   directory)
		    :overwrite t)
  (cl-fad:copy-file (project-path "MathJax.js" :resources) 
		    (merge-pathnames "MathJax.js" directory)
		    :overwrite t)
  (copy-directory-recursively (project-path "extensions/" :resources)
			      (merge-pathnames "extensions/" directory))
  (copy-directory-recursively (project-path "images/" :resources)
			      (merge-pathnames "images/" directory)))

(defun html-dirs-for-organized (org directory)
  (write-additional-files directory)
  (write-html-index org directory)
  (fset:do-map (key value org)
    (declare (ignore value))
    (html-for-chapter org key directory :active-words org)))

(defun html-for-organized (org &optional (stream *standard-output*))
  (fset:do-map (key value org)
    (lml2:html-print `(:h1 ,key) stream)
    (loop :for entry :in value :do
       (html-for-entry entry :stream stream))))


(defun html-for-organized-file (org file-name)
  (with-open-file (s file-name :direction :output :if-exists :supersede)
    (html-for-organized org s)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct class-description 
  class-name
  class-entry
  method-entries
  property-entries
  function-entries)

(defun bag-classes (list)
  (let ((result (fset:map)))
    (loop :for entry :in list :do
       (when-let (class (car (is-part-of-class entry)))
	 (let ((cd (or (fset:lookup result (name-of-class entry))
			  (make-class-description :class-name (name-of-class entry)))))
	   (case class
	     (#\c (setf (class-description-class-entry cd) entry))
	     (#\m (push entry (class-description-method-entries cd)))
	     (#\p (push entry (class-description-property-entries cd)))
	     (#\f (push entry (class-description-function-entries cd))))
	   (setf result (fset:with result (name-of-class entry) cd)))))
    result))

(defun bag-classes-and-functions (list)
  (let ((result (fset:map)))
    (loop :for entry :in list :do
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


(defun source-dir-to-html-classes (&key 
				     (sources "d:/Sources/StorageMagic/Source/CppSource/")
				     (html "d:/Weeks/52/html/"))
  (html-dirs-for-organized 
   (bag-classes-and-functions
    (read-directory-and-collect-robodoc-entries sources))
   html))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST XML PARSING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass test-handler (cxml:sax-proxy)
  ((in-use :initform nil :accessor in-use)))

(defmethod sax:characters ((handler test-handler) (data t))
  (if (in-use handler)
      (call-next-method handler data)
      (progn
#+nil	(setf (in-use handler) t)
	(sax:start-element handler  nil nil "a" (list (sax:make-attribute :qname "xlink:href"
									  :value (format nil "~A.html" data))))
	(call-next-method  handler data)
	(sax:end-element handler nil nil "a")
#+nil	(call-next-method handler "En verder")
#+nil	(setf (in-use handler) nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun linkify (in-stream out-stream)
  (let ((th (make-instance 'test-handler))
	(sink (cxml:make-character-stream-sink out-stream)))
    (push sink (cxml:broadcast-handler-handlers th))
    (cxml:parse in-stream th)))

(defun test-xml () 
  (let ((th (make-instance 'test-handler))
	(sink (cxml:make-character-stream-sink *standard-output*)))
    (push sink (cxml:broadcast-handler-handlers th))
    (cxml:parse #P"d:/tmp/2045842259.svg" th)))
