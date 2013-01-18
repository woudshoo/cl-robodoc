(in-package #:cl-robodoc)

(defatom identifer-char? (c)
  (not (or (white-space? c) (char-equal #\/ c) (newline? c) (char-equal #\Return c))))
(defatom liberal-whitespace? (c)
  (or (white-space? c) (char-equal #\Return c)))

(defatom non-newline? (c)
  (and (not (char-equal #\Return c)) (not (char-equal #\Newline c))))

;;; TOPLEVEL PARSING, parses the file in chunks to get
;;;                   the comments and the text section


;;; comment-start-rd?
;;;
;;; Parses a robodoc comment start.
;;; This is a comment starting looking like:
;;;
;;;  /****[type-char][subtype-char]* [Chapter-Level-String]<optionaly: "/" [Subchabter string]
;;;  
;;; It returns a list:
;;;
;;;   (:RD :type [type-char] :subtype [subtype-char] :first [Chapter-level-string] :second [Subchapter string]
;;;
(defrule comment-start-rd? (&aux type subtype first second tmp) ()
  "/****"
  (:* "*")
  (:assign type (:type alpha?))
  (:? (:assign subtype (:type alpha?)))
  "*"
  (:+ (:type white-space?))
  (:assign first (make-char-accum))
  (:+ 
   (:assign tmp (:type identifer-char?))
   (:char-push tmp first))
  (:? "/"
      (:assign second (make-char-accum))
      (:+ 
       (:assign tmp (:type identifer-char?))
       (:char-push tmp second)))
  (:* (:type liberal-whitespace?))
  (:type newline?)
  (:return (list :RD :type type :subtype subtype :first first :second second)))

(defrule comment-start? (&aux result) ()
  (:or 
   (:and (:assign result (:rule comment-start-rd?)) (:return result))
   (:and "/*" (:return :SIMPLE))))

(defrule comment-end? () ()
  (:or 
   (:and "***/" (:return :RD))
   (:and "*/" (:return :NORMAL))))

(defrule comment? (&aux (result (make-char-accum)) comment-start comment-end) ()
  (:assign comment-start (:rule comment-start?)) 
  (:+ (:not (:rule comment-end?))
      (:char-push result))
  (:assign comment-end (:rule comment-end?)) 
  (:return (cons  (list :comment :start comment-start :end comment-end) result )))

(defrule non-comment? (&aux (result (make-char-accum))) ()
  (:+ (:not "/*")
      (:char-push result))
  (:return (cons :TEXT result)))

(defrule file-comments? (&aux (result (make-list-accum)) comment) ()
  (:* 
   (:assign comment (:or (:rule comment?) (:rule non-comment?)))
   (:list-push comment result))
  (:return (reverse result)))


;;;; parsing the text sections
;;;; -------------------------
;;;;
;;;; Parses the text section in to 'sections' and 'lists' etc.
;;;;
;;;; This does not parse the 'text' sections.  
(defrule ignore-marker? () ()
  (:? (:* (:type white-space?))) (:+ "*"))

(defrule heading? (&aux (result (make-char-accum)) char) ()
  (:* (:type white-space?))
  (:+ (:assign char (:type upper?))
      (:char-push char result))
  (:* (:type liberal-whitespace?))
  (:type newline?)
  (:return (cons :H1 result)))

(defrule normal-line? (&aux (result (make-char-accum)) char) ()
  (:* (:assign char (:type non-newline?))
      (:char-push char result))
  (:* (:type liberal-whitespace?))
  (:type newline?)
  (:return result))

(defrule new-par? () ()
  (:* (:type liberal-whitespace?))
  (:type newline?)
  (:return :PAR))

(defrule content? (&aux (result (make-list-accum)) line) ()
  (:*
   (:and (:rule ignore-marker?)
	 (:assign line (:or (:rule new-par?) (:rule heading?) (:rule normal-line?)))
	 (:list-push line result)))
  (:return (cons :CONTENT (reverse result))))

;(defrule item? (&aux (result (make-char-accum))))

(defun parse-c-file (in-file-name)
  (file-comments? (create-parser-context (alexandria:read-file-into-string in-file-name))))




