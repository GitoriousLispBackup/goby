(in-package :goby)
(defun dot-split (str) (cl-irregsexp:match-split "." str))
(defun last1 (lst) (first (last lst)))
(defun length1 (lst) (and (not (null lst)) (null (cdr lst))))
(defun mklst (lst) (if (listp lst) lst (list lst)))
(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

(defun strcat (&rest args) (apply #'concatenate 'string args))

(defun with-all (initial-map bindings)
  (iter
    (for bind in bindings)
    (for map initially (fset:with initial-map (first bind) (second bind)) then (fset:with map (first bind) (second bind)))
    (finally (return map))))


(define-modify-macro strcatf (&rest args) strcat)
(defun symbolicate (&rest symbols)
  (read-from-string
   (iter
    (with result = "")
    (for sym in symbols)
    (strcatf result (string sym))
    (finally (return result)))))
