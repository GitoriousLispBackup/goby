(in-package :goby)
(defun last1 (lst) (first (last lst)))
(defun length1 (lst) (and (not (null lst)) (null (cdr lst))))
(defun mklst (lst) (if (listp lst) lst (list lst)))
(defun set! (var val) `(_= ,var ,val))
(defun strcat (&rest args) (apply #'concatenate 'string args))

