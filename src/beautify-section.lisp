(in-package :cl-robodoc)


(defun char-is-leader (char)
  (or (eql char #\*) (eql char #\Space)
      (eql char #\Return)))

(defstruct parse-block 
  parent
  (level -1)
  type
  (content (list)))


(defun simple-type (type)
  "Returns the 'tag' of the type.  
A type is one of two forms:

 - TAG  -- indicating the type
 - (TAG attributes) -- List with TAG indicating the type.

This function returns the TAG."
  (if (consp type) (car type) type))

(defun last-indent (line-indent)
  "Returns the last indentation that occured.  Line indent is a list
of indent positions.  This will return the increase of indent of the
current indent vs the previous indent."
  (- (car line-indent) (or (cadr line-indent) 0)))

(defun line-content-for-type (line type line-indent)
  "Returns the line content to add to the output, depending on the type and read line.
The input line does not contain the initial skipped charactes,
such as '*' at the beginning of the line, or spaces.

However it still contain characters such as '-' to indicate the beginning of a list item.
This function returns the line without the ascii marker '-'.
"
  (case (simple-type type)
    ;;; Remove the '- ' for list items
    (:li (subseq line 2))
    ;;; Remove final #\Newline and spaces from titles
    ((:h1 :h2 :h3) (string-trim '(#\Space #\Newline) line))
    ;;; Reinsert the spaces before verbatim lines
    (:pre 
     (concatenate 'string (make-string (last-indent line-indent) :initial-element #\Space) line))
    (:uml
     (subseq line #.(length "#+uml")))
    (:gnuplot
     (subseq line #.(length "#+plot")))
    ;;; Default pass unmodfied
    (t line)))

(defun block-to-lml (p-block)
  (loop :with type = (parse-block-type p-block)
     :for line :in (reverse (parse-block-content p-block))
     :finally (return (if (not type) result (cons type result)))
     
     :when  (typep line 'parse-block) :collect (block-to-lml line) :into result
     :when (and type (not (typep line 'parse-block))) :collect line :into result))

(defmethod beautify ((text string) (type (eql :comment)))
  "We will try to parse the text as a kind of markup language.
For this we need sometimes to open blocks which we can close latere, e.g.
lists of items which can be nested.

So the blocks are a stack and lines are added to the top most stack entry.
The close method will write the blocks to the result list.  
A block consists of the following information:
 - Level, used to know if we need to close the element
 - Type, used to construct the block
 - content"
  (let (char (line-indent (list -1))
	     current-line 
	     (column 0)
	     (parse-state :skipping-initial)
	     (blocks (list (make-parse-block))))

    (labels ((update-line-indent (column)
	       (loop :while (and line-indent (>= (car line-indent) column))
		  :do (pop line-indent))
	       (push column line-indent))

	     (current-block () (car blocks))
	     (current-block-level () (parse-block-level (current-block)))
	     (current-line-level () (car line-indent))
	     (add-child-block (type initial-content)
	       (let ((new-block (make-parse-block :parent (current-block)
						  :level (current-line-level)
						  :type type
						  :content initial-content)))
		 (push new-block (parse-block-content (current-block)))
		 (push new-block blocks)))

	     (line-indicates-li (line) 
	       (and (> (length line) 3)
		       (member (char line 0) '(#\- #\o #\*))
		       (eql (char line 1) #\Space)))

	     (line-indicates-ordered-li (line) 
	       (and (> (length line) 3)
		    (cl-ppcre:scan "^[0-9]+\\. " line)))

	     (type-for-new-block (line)
	       (cond
		 ((eql (length line-indent) 2)
		  (list :h3 :class (line-content-for-type line :h3 line-indent)))

		 ((line-indicates-li line) '(:li :ul))
		 ((line-indicates-ordered-li line) '(:li :ol))
		 ((starts-with-subseq "#+uml" line) :uml)
		 ((starts-with-subseq "#+plot" line) :gnuplot)
		 ((starts-with-subseq "`" line) '(:p :class "display-math"))
		 ((> (length line-indent) 3) :pre)

		 ((= 0 (length (remove #\Space (remove #\Newline line)))) :br)



		 (t :p)))


	     (simple-type-for-line (line)
	       (simple-type (type-for-new-block line)))

	     (inline-block-p (line)
	       (cond
		 ((eql (simple-type-for-line line) :h3) t)
		 ((eql (simple-type-for-line line) :br) t)
		 (t nil)))

	     (determine-next-li-indent (line)
	       (if-let (position (search " -- " line))
		 (+ 4 position)
		 (if (line-indicates-ordered-li line)
		     3
		     2)))

	     (starts-new-block-p (line)
	       (cond 
		 ((line-indicates-li line) t)
		 ((line-indicates-ordered-li line) t)
		 (t nil)))

	     (block-can-be-closed-p (new-type line-level block)
	       "Determine the block can be closed depending on the new-type of block
and the level.  
Special cases are the 'pre' tags."
	       (declare (ignore block))
	       (case (simple-type new-type)
		 (:pre nil)
		 (t (> (current-block-level) line-level))))

	     (close-blocks-if-needed (new-type)
	       ;; Pop elements from stack based upon indentation level
	       (loop :while (block-can-be-closed-p new-type (current-line-level) (current-block))
		  :do (pop blocks))

	       (let ((old-type (parse-block-type (current-block))))
		 (when  (or  
			 ;; If a non list item is added to list environment, close the list environment
			 (and (not (eql (simple-type new-type) :li)) 
			      (member old-type '(:ul :ol)))
			 ;; If a paragraph is following a paragraph, close previous paragraph
			 (and (eql new-type :br) (eql (simple-type old-type) :p))) 
		   (pop blocks))))

	     (merge-with-current-block-p (new-type)
	       (let ((old-type (parse-block-type (current-block))))
		 (cond
		   ((and (eql :pre new-type) (eql :pre old-type)) t)
		   ((and (eql :pre new-type) (eql :uml old-type)) t)
		   ((and (eql :pre new-type) (eql :gnuplot old-type)) t)
		   (t nil))))

	     (ignore-element (type line)
	       (case (simple-type type)
		   (:p nil)
		   (:br 
		    (member (simple-type (parse-block-type (current-block)))
				'(nil :p :h1 :h2 :h3 :uml :gnuplot :h4 :li :pre)))
		   (t (zerop (length line)))))

	     (get-enclosing-ul/ol-block ()
	       "Returns either the current block or the parent block if it is a :ul block"
	       (cond
		 ((not (current-block)) nil)
		 ((member (parse-block-type (current-block)) '(:ul :ol)) (current-block))
		 ((and (parse-block-parent (current-block))
		       (member (parse-block-type (parse-block-parent (current-block))) '(:ul :ol)))
		  (parse-block-parent (current-block)))
		 (t nil)))

	     (add-parent-block-if-needed (type)
	       "Method to insert implicitly requested parent blocks.
Currently I think this is wrong.  However we need to see how to fix this later."
	       (when (eql (simple-type type) :li)
		 (let ((ul-block (get-enclosing-ul/ol-block)))
		   (when (or (not ul-block)
			     (> (current-line-level) (parse-block-level ul-block)))
		     (add-child-block (second type) (list))))))
	     
	     (add-line-to-blocks (line)
	       (let* ((new-type (type-for-new-block line))
		      (new-line (line-content-for-type line new-type line-indent)))

		 (close-blocks-if-needed new-type)

		 (when (ignore-element new-type new-line)
		   (return-from add-line-to-blocks))

		 (when (merge-with-current-block-p new-type)
		   (push new-line (parse-block-content (current-block)))
		   (return-from add-line-to-blocks))


		 (add-parent-block-if-needed new-type)

		 (when (or  (line-indicates-li line)
			    (line-indicates-ordered-li line))  
		   (incf (car line-indent) (determine-next-li-indent line)))   

		 (cond 
		   ((inline-block-p line) 

		    (add-child-block new-type (list new-line))
		    (pop blocks))
		   
		   ((> (current-line-level) (current-block-level))

		    (add-child-block new-type (list new-line)))

		   ((= (current-line-level) (current-block-level))

		    (if (starts-new-block-p line)
			(progn

			  (pop blocks)
			  (add-child-block new-type (list new-line)))
			(progn 	
			  (push new-line (parse-block-content (current-block))))))))))


      (with-input-from-string (in text)
	(tagbody
	   
	 :next-char
	   (when (eql char :eof) 
	     (return-from beautify #+nil (lastcar blocks) #-nil (block-to-lml (lastcar blocks))))
	   (setf char (read-char in nil :eof))
	   (incf column)
	   (when (eql char #\Return) (go :next-char)) ;; Complete ignore returns
	   (when (eql char #\Newline) (push char current-line))
	   (when (or (eql char #\Newline) (eql char :eof)) 
	     (add-line-to-blocks (coerce (reverse current-line) 'string))
	     (setf parse-state :skipping-initial)
	     (setf column 0)
	     (setf current-line nil)
	     (go :next-char))

	   (when (eql parse-state :skipping-initial)
	     (when (member char '(#\* #\Space)) (go :next-char)) ;; Skip characters at beginning of line
	     (update-line-indent column)                        ;; Remember where the line starts
	     
	     (setf parse-state :start-data))
	   
	   (push char current-line)  ;; collect curent char
	   (go :next-char))))))


(defmethod beautify ((text string) (type (eql :text)))
  (list 
   (list :pre
	 (list :ccode text))))

(defun beautify-section (section)
  "Returns a 'beautified' version.  The input is a string, the result is a 'lml' formatted output."
  (beautify (remove #\Return (cdr section)) (car section)))
