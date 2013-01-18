;;;; cl-robodoc.lisp

(in-package #:cl-robodoc)

;;; "cl-robodoc" goes here. Hacks and glory await!



;;; The idea is to make a robodoc parser which parses
;;; at least the subset we use in storage magic
;;;
;;; There are two parts to this:
;;;
;;;  1. collect the comment sections relevant to robodoc
;;;  2. parse the free text into sementic parts somewhat similar to asciidoc or markdown.
;;;
;;; we should check if markdown is similar enough to actually use that part to parse
;;; the free text.
;;;
;;;
;;; 1. parsing the sections
;;; -----------------------
;;; Sections are delimited between
;;;
;;;  1. start regular expresion:  e.g. \*\*\*\*??\*
;;;  2. end regular expression e.g. \*\*\*\*
;;;
;;; In such a section only certain lines are accepted
;;; These lines should be marked between
;;;
;;; begin markers and end markers.
;;;
;;; In c:
;;;
;;; begin marker:  /*
;;; end marker:    */
;;;
;;; begin marker: //
;;; end marker:  end of line
;;;
;;; Also some characters at the beginning of a line are stripped, such as *
;;;
;;; maker: *
;;;
;;;
;;;
;;; So given this we can parse it into a list of marks and others:
;;;
;;; ((:header ....) (:remark ...) (:other ..) (:remark ...))
;;;
;;; which is basically a breakdown of the file:
;;;  


