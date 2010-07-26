
(defpackage :modern
  (:use :iterate :cl)
  (:export copy-package))
(in-package :modern)

(defun macro? (a) (macro-function a))
(defun bound? (a) (boundp a))
(defun function? (a) (fboundp a))
(defun copy-sym (from to)
  (cond
    ((macro? from)
     (setf (macro-function to)
	   (macro-function from)))
    ((bound? from)
     (setf (symbol-value to)
	   (symbol-value from)))
    ((function? from)
     (setf (symbol-function to)
	   (symbol-function from)))))

(defun copy-package (from to)
  (iter 
    (for symbol in-package from external-only t)
    (for sym = (intern (string-downcase (string symbol))
		       to))
    (copy-sym symbol sym)
    (export sym to)))





