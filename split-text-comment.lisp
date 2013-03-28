(in-package :cl-robodoc)

;;; Code to split a C like code stream into Comments and Text.


(defclass section-splitter ()
  ((in-stream :initarg :in-stream :reader in-stream)
   (state :accessor state :initform :text :documentation "value is :comment, :text of :eof"))
  (:documentation "Class that will split the stream `in-stream' representing a mix of source and 
commentes into sections.
A section is either a :text or a :comment section, with :text section representing code, and
:comment section containing the comments.

Sections are presented as cons cells with the first element the type and the second element the content.

e.g.:  
\"Hallo/* Daar */\"

Will be split  into: (:text . \"Hallo\")  (:comment \" Daar \")

Note that this class will only recogize /* */ c-style comments, //
comments are considered text.
"))

(defmethod next ((splitter section-splitter))
  "Returns the next section of the stream being split by `splitter'."
  (read-next splitter (state splitter)))


(defmethod read-next ((splitter section-splitter) (state (eql :text)))
  (cons :text
	(with-output-to-string (result)
	  (loop :named parse-loop
	     :with stream-state = :normal
	     :for char = (read-char (in-stream splitter) nil :eof)
	     :do
	     (when (eql char :eof)
	       (setf (state splitter) :eof)
	       (return-from parse-loop)) 
	     (case stream-state 
	       (:normal (case char
			  (#\/ (setf stream-state :bs))
			  (#\\ (setf stream-state :s) (write-char char result))
			  (#\" (setf stream-state :string) (write-char char result))
			  (t (write-char char result))))
	       (:s (setf stream-state :normal) (write-char char result))
	       (:string (case char
			  (#\" (setf stream-state :normal) (write-char char result))
			  (#\\ (setf stream-state :s-string) (write-char char result))))
	       (:bs (case char
		      (#\* (setf (state splitter) :comment)
			   (return-from parse-loop))
		      (t (setf stream-state :normal) 
			 (write-char #\/ result)
			 (write-char char result))))
	       (:s-string (setf stream-state :string) (write-char char result)))))))


(defmethod read-next ((splitter section-splitter) (state (eql :comment)))
  (cons :comment
	(with-output-to-string (result)
	  (loop :named parse-loop
	     :with stream-state = :normal
	     :for char = (read-char (in-stream splitter) nil :eof)
	     :do
	     (when (eql char :eof)
	       (setf (state splitter) :eof)
	       (return-from parse-loop))
	     (case stream-state
	       (:normal (case char
			  (#\* (setf stream-state :star))
			  (t (write-char char result))))
	       (:star (case char
			(#\/ (setf (state splitter) :text)
			     (return-from parse-loop))
			(#\* (write-char #\* result))
			(t (setf stream-state :normal)
			   (write-char #\* result)
			   (write-char char result)))))))))

(defmethod read-next ((splitter section-splitter) (state (eql :eof)))
  nil)




;;;;;;;;

(defmethod next-comment-block ((splitter section-splitter))
  "Like next, but it will skip the text blocks."
  (loop :for block = (next splitter)
     :until (or (not block) 
		(eql (car block) :comment))
     :finally (return block)))
