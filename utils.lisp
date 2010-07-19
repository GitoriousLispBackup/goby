(in-package :goby)
(defun last1 (lst) (first (last lst)))

(defmethod str ((str string)) str)
(defmethod str ((sym symbol)) (string sym))
(defmethod str (obj) (write-to-string obj))
(defun strcat (&rest args) (apply #'concatenate 'string args))
(defun mklst (lst) (if (listp lst) lst (list lst)))
(defmacro  modify! (class &rest args)
  (let ((gen-class (gensym)))
    `(let ((,gen-class ,class))
       (setf
	,@(iter (for (key val) on args by #'cddr)
		(appending `((slot-value ,gen-class ',(read-from-string (string key))) ,val))))
       ,gen-class)))

