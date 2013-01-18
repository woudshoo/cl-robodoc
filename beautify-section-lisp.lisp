(in-package :cl-robodoc)



(defun type-of-fragment (fragment)
  (if (symbolp fragment) fragment (type-of-fragment (car fragment))))

(defun content-of-fragment (fragment)
  (cdr fragment))

(defun type-tree-of-fragment (fragment &key (nils-p t))
  (when (listp fragment)
    (cons (type-of-fragment fragment)
	  (loop :for sub :in (rest fragment) 
	     :for subtree = (type-tree-of-fragment sub)
	     :when (or nils-p subtree) :collect subtree))))

(deftestsuite beautify-simple ()
  ((headers-and-text 
    " * DIT IS A HEADER
 * 
 *   And this is a section
 *
 * HEADER 2
 *   more text"))
  (:tests

   (headers-count 
    (ensure (= (length (beautify headers-and-text :comment))  4)))

   (first-entry-is-header
    (ensure-same (type-of-fragment 
		  (first 
		   (beautify headers-and-text :comment))) 
		 :h3))

   (second-is-normal
    (let ((sec (second (beautify headers-and-text :comment))))
      (ensure-same (type-of-fragment sec) :p)
      (ensure-same (content-of-fragment sec) (list "And this is a section
"))))

   (types-match
    (ensure-same (mapcar #'type-of-fragment (beautify headers-and-text :comment))
		 '(:h3 :p :h3 :p)))))





(defun test-type-tree (fragment type-tree &optional description)
  (ensure-same (mapcar #'type-tree-of-fragment (beautify fragment :comment))
	       type-tree
	       :report description))
    
(deftestsuite beautify-lists-1 ()
  ((list-items-1
    " * HEADER
 *   text
 *   - Item 1
 *   - Item 2
 * HEADER")
   (list-items-2
    " - Item
 - Item")
   (list-items-3
    " CHAPTER
   - Item")
   (list-items-4
    " CHAPTER
   - Item level 1
     continuation text
   - Item level 2")
   (list-items-5
    " CHAPTER
   - Item
   Normal
   - Item")
   (list-items-6
    " CHAPTER
   - Item
   
   Normal text some more
   what is next?

   - Item")
   
   (list-items-7
    " CHAPTER
  - Item
    - Sub Item
  - Item"))
  
  (:tests

   (types-match-1 
    (test-type-tree list-items-1 '((:h3 nil) (:p nil  (:ul (:li nil) (:li nil))) (:h3 nil))
		    "Simple normal text with a list"))

   (types-match-2
    (test-type-tree list-items-2 '((:h3 nil) (:h3 nil))
		    "Check that - at chapter level does not create a list"))

   (types-match-3
    (test-type-tree list-items-3 '((:h3 nil) (:ul (:li nil)))
		    "Check that list can be first thing in a chaper"))
   
   (types-match-4 
    (test-type-tree list-items-4 '((:h3 nil) (:ul (:li nil nil) (:li nil)))
		    "Test simple continuation of list item on next line"))
   
   (types-match-5
    (test-type-tree list-items-5 '((:h3 nil) (:ul (:li nil)) (:p nil (:ul (:li nil))))
		    "Test that normal text stops the list"))

   (types-match-6
    (test-type-tree list-items-6 '((:h3 nil) (:ul (:li nil)) (:p nil nil (:ul (:li nil))))
		    "Test that normal text stops the list"))
   (types-match-7
    (test-type-tree list-items-7 '((:h3 nil) (:ul (:li nil (:ul (:li nil))) (:li nil)))
		    "Test nested lists work"))))
