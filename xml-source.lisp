(in-package :cl-robodoc)


(defun type-of-fragment (fragment)
  (if (symbolp fragment) fragment 
      (type-of-fragment (car fragment))))

(defun content-of-fragment (fragment)
  (cdr fragment))

(defun attributes-of-fragment (node)
  (when (consp (car node))
    (cdar node)))

(defun compute-attributes (node)
  (loop :for (key value) :on (attributes-of-fragment node) :by #'cddr 
     :collect
     (sax:make-attribute :qname (tag-name key)
			 :value (tag-name value))))
     

(defun tag-name (name)
  (typecase name
    (string name)
    (symbol (string-downcase (symbol-name name)))))

(defun map-node (handler node &optional (start-node t))
  (when start-node 
    (sax:start-document handler))
  (typecase node
    (list
     (sax:start-element handler nil nil 
			(tag-name (type-of-fragment node))
			(compute-attributes node))
     (dolist (child (content-of-fragment node))
       (map-node handler child nil))
     (sax:end-element handler nil nil (tag-name (type-of-fragment node))))
    (string (sax:characters handler node)))
    (when start-node 
      (sax:end-document handler)))

		     
