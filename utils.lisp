(in-package :goby)
(defun last1 (lst) (first (last lst)))
(defun length1 (lst) (and (not (null lst)) (null (cdr lst))))
(defun mklst (lst) (if (listp lst) lst (list lst)))
(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

(defun strcat (&rest args) (apply #'concatenate 'string args))

(define-modify-macro strcatf (&rest args) strcat)
(defun symbolicate (&rest symbols)
  (read-from-string
   (iter
    (with result = "")
    (for sym in symbols)
    (strcatf result (string sym))
    (finally (return result)))))
