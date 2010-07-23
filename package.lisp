(defpackage :goby (:use :cl :iterate) (:shadow :cl :let))
(in-package :goby)
(defmacro let (&rest args) `(metabang-bind:bind ,@args))

(defvar *false* '|False|)
(defvar *true* '|True|)
(set-dispatch-macro-character
 #\# #\f (lambda (stream char1 char2) `(quote ,*false*)))
(set-dispatch-macro-character
 #\# #\t (lambda (stream char1 char2) `(quote ,*true*)))
