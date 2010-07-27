(defpackage :goby (:use :cl :iterate :iolib) (:shadow :cl :let) (:export :py :unroll :defmac))
(in-package :goby)

(defmacro let (&rest args) `(metabang-bind:bind,@args))

(defvar *default* '|None|)
(defvar |False| '|False|)
(defvar |True| '|True|)

(set-dispatch-macro-character
 #\# #\f (lambda (stream char1 char2) '|False|))
(set-dispatch-macro-character
 #\# #\t (lambda (stream char1 char2) '|True|))


