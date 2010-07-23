(in-package :goby)
#|
(defmac while (test &rest body)
  `(progn (_while (= 1 1) (if ,test (break)) ,@body) ,*default*))
(defmac for (clauses &rest body)
  `(progn (_for ,clauses ,@body) ,*default*))
|#
(defmac let (bindings &rest body)
  `(progn ,@(iter
	     (for (var val) in bindings)
	     (collecting `(setq ,var ,val))) ,@body))
;;TODO: make into what a normal python user would use!
(defmac or (&rest forms &aux (first-gen (gensym)))
  (if (null forms)
      #f
      `(let ((,first-gen ,(first forms)))
	 (if ,first-gen
	     ,first-gen
	     (or ,@(cdr forms))))))

(defmac and (&rest forms &aux (first-gen (gensym)))
  (if (null forms)
      #f
      `(let ((,first-gen ,(first forms)))
	 (if ,first-gen
	     (and ,@(cdr forms))
	     #f))))
(defmac import (&rest args) `(_import ,@args))
(defmac from (&rest args) `(_from ,@args))
(defmac when (test &rest body) `(if ,test (progn ,@body)))
;;todo:add not as unary operator to make translation more pythonic?
(defmac unless (test &rest body) `(if (not ,test) (progn ,@body)))