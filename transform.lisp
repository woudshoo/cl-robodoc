(in-package #:cl-robodoc)



(defun tag (entry)
  "Returns the tag of an entry.  For this the tag is a symbol and
either is directly supplied or at the car of the entry.  
This is done recursively, so (((((:SYMBOL ...  will return :SYMBOL as tag."
  (etypecase entry
    (list (tag (car entry)))
    (symbol entry)))

(defun tag= (tag entry)
  "Returns true if the entry has tag tag"
  (eq tag (tag entry)))

(defun attribute-value (attr entry)
  "Returns the value of the attribute attr.
It will return nil if the attribute does not exist."
  (when (listp (car entry))
    (getf (cdar entry) attr)))



(defun content-entry (entry)
  (cdr entry))


(defun rd-start? (entry)
  (and (tag= :COMMENT entry)
       (tag= :RD (attribute-value :START entry))))

(defun rd-end? (entry)
  (and (tag= :COMMENT entry)
       (tag= :RD (attribute-value :END entry))))

(defun select-rd (raw-code)
  "Returns a list of rd sections.  A rd section
is just a list of :COMMENT and :TEXT sections, and is not procesed in any way.
An RD section is identified by a :COMMENT section which is of start type :RD and ended
by a :COMMENT section which is of end type :RD."
  (loop 
     :with result = (list)
     :with in-rd = nil
     :for entry :in raw-code 
     :finally (return (let ((real-result (list)))
			(mapcar #'(lambda (v) (push (reverse v) real-result)) result)
			real-result))
     :do
     (when (rd-start? entry)
       (when in-rd (error "Found new start while previous RD entry was not closed"))
       (setf in-rd t)
       (push (list) result))
     (when in-rd
       (push entry (first result)))
     (when (rd-end? entry)
       (unless in-rd (error "Found close while no RD entry is open"))
       (setf in-rd nil))))



(defun parsed-rd (rd-entry)
  (loop :with result = (list)
     :for entry :in rd-entry
     :finally (return (reverse result))
     :do
     (cond
       ((tag= :COMMENT entry)
	(push (content? (create-parser-context (content-entry entry))) result))
       (t (push entry result)))))