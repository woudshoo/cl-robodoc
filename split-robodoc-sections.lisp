(in-package :cl-robodoc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ROBODOC section are lists containing 3 elements:
;;
;; (:robodoc header sections)
;;
;; with:
;;   - :robodoc  indicating that this is a robodoc section
;;   - header:  Al ist containing two elements:
;;              - type list
;;              - name list
;;   - sections:  A list of raw sections (without the starter and ender)
;;                raw sections are basically just the plain file content.
;;


(defun robodoc-header (robodoc-entry)
  "Returns the full header of a robodoc entry"
  (second robodoc-entry))

(defun robodoc-entry-p (robodoc-entry)
  "Returns t if it is ROBODOC entry"
  (eql (car robodoc-entry) :robodoc))

(defun classification (entry)
  "Returns the classification list of a robodoc entry"
  (and (robodoc-entry-p entry)
       (car (robodoc-header entry))))

(defun names (entry)
  "Returns the list of names of a robodoc entry"
  (and (robodoc-entry-p entry)
       (cdr (robodoc-header entry))))

(defun simple-name (entry)
  "Returns a single string indicating the name."
  (and (robodoc-entry-p entry)
       (lastcar (second entry))))

(defun is-class (entry)
  "Entry is a class entry"
  (member #\c (classification entry)))

(defun is-method (entry)
  "If the entry indicates a method"
  (member #\m (classification entry)))

(defun is-property (entry)
  "If the entry indicates a method"
)


#+nil (defun is-part-of-class (entry)
  "Returns if the robodoc entry is either a class or a method. (or later maybe a property etc.)"
  (intersection (classification entry) '(#\c #\m #\p)))

#+nil (defun name-of-class (entry)
  "Returns a string indicating the class name if it is a class."
  (when (is-part-of-class entry)
    (cond 
      ((is-class entry) (lastcar (names entry)))
      ((or (is-method entry) (is-property entry)) (first (names entry)))
      (t (error "Cannot determine class name")))))

(defun name-of-group (entry)
  "Name of a group.  Typically a class name"
  (cond
    ((is-class entry) (lastcar (names entry)))
    (t (first (names entry)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass robodoc-splitter ()
  ((section-splitter :initarg :section-splitter :reader section-splitter)
   (state :accessor state :initform :normal :documentation "value is :normal, :???")
   (c-stream :accessor c-stream :initform nil)))


(defmethod comment-stream ((splitter robodoc-splitter))
  (with-slots (c-stream) splitter
    (unless c-stream
      (when-let (next-block (next-comment-block (section-splitter splitter)))
	(setf c-stream 
	      (make-string-input-stream (cdr next-block)))))
    c-stream))


(defun scan-until-3-stars (stream)
  (loop 
     :with star-count = 0
     :for char = (read-char stream nil :eof)
     :until (or (eql char :eof) (> star-count 2))
     :finally (return (unless (eql char :eof) 
			(unread-char char stream)
			t))
     :do
     (if (eql #\* char)
	 (incf star-count)
	 (setf star-count 0))))

(defun skip-character (stream char-to-skip)
  (loop 
     :for char = (read-char stream nil :eof)
     :while (eql char char-to-skip)
     :finally (return (unless (eql char :eof)
			(unread-char char stream)
			t))))

(defun skip-stars (stream)
  (skip-character stream #\*))


(defun valid-modifier-char (char)
  (let ((code (char-code char)))
    (and (<= (char-code #\a) code)
	 (<= code (char-code #\z)))))

(defun valid-name-char (char)
  (not (or (eql #\Space char) 
	   (eql :eof char)
	   (eql #\Newline char)
	   (eql #\Return char))))

(defun scan-modifiers (stream)
  (loop :for char = (read-char stream nil :eof)
     :while (valid-modifier-char char)
     :collect char))

(defun scan-names (stream modifiers)
  (skip-character stream #\Space)
  (when-let (name
	     (loop :for char = (read-char stream nil :eof)
		:while (valid-name-char char)
		:collect char))
    (cons modifiers (split-sequence:split-sequence #\/ (coerce name 'string)))))


(defun scan-modifiers-and-names (stream)
  (when-let (modifiers (scan-modifiers stream))
    (and (skip-stars stream)
	 (scan-names stream modifiers))))


(defun find-header (stream)
  (when stream
    (and (scan-until-3-stars stream)
	 (skip-stars stream)
	 (scan-modifiers-and-names stream))))

(defmethod scan-up-to-header ((splitter robodoc-splitter))
  (loop :for comment-stream = (comment-stream splitter)
     :for header = (find-header comment-stream)
     :while (and comment-stream (not header))
     :finally (return header)
     :do (setf (c-stream splitter) nil)))


(defun next-part-by-parsing-comment-stream (splitter)
  (cons :comment
	(loop :with star-count = 0 
	   :with out = (make-string-output-stream)
	   :for char = (read-char (c-stream splitter) nil :eof)
	   :until (or (eql char :eof) (> star-count 4))
	   :finally (return 
		      (progn (if (eql char :eof)
			       (setf (c-stream splitter) nil)
			       (setf (state splitter) :end-section))
			     (get-output-stream-string out)))
	   :do 
	   (if (eql char #\*) 
	       (incf star-count)
	       (setf star-count 0))
	   (write-char char out))))


(defmethod next-part ((splitter robodoc-splitter))
  (if (eql (state splitter) :end-section)
      (progn 
	(setf (state splitter) :normal)
	nil)
      (if-let (c-stream (c-stream splitter))
	(next-part-by-parsing-comment-stream splitter)
	(let ((next-block (next (section-splitter splitter))))
	  (case (car next-block)
	    (:comment (setf (c-stream splitter) (make-string-input-stream (cdr next-block)))
		      (next-part-by-parsing-comment-stream splitter))
	    (:text next-block))))))

(defmethod next ((splitter robodoc-splitter))
  (when-let (header (scan-up-to-header splitter))
    (list :robodoc  header
	  (loop :for part = (next-part splitter)
	     :while part :collect part))))

