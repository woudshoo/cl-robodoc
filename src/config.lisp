(in-package :cl-robodoc)

;; code to setup the configuration of external tools, paths etc.
;;

(defparameter *uml-jar* #-darwin "d:/Tools/bin/plantuml.jar"
	      #+darwin "/Users/woudshoo/bin/plantuml.jar")


(defparameter *gnuplot-cmd* "gnuplot")
(defparameter *java-cmd* "java")


(defmacro update-parameter (config key parameter)
  `(alexandria:when-let ((value (alexandria:assoc-value ,config ,key)))
     (setf ,parameter value)))

(defun read-config-file (&optional file-name)
  (let ((file-name (or file-name "~/.cl-robodoc.conf")))
    (handler-case
	(with-open-file (in file-name)
	  (let ((config (read in)))
	    (update-parameter config :java-cmd *java-cmd*)
	    (update-parameter config :gnuplot-cmd *gnuplot-cmd*)
	    (update-parameter config :plantuml-jar *uml-jar*)))
      (file-error ()))))
