(in-package :goby)

(defmac fn (args &rest body)
  `(def ,(gensym "lambda") ,args ,@body))

(defmac let! (bindings &rest body)
  `(progn ,@(iter
	     (for (var val) in bindings)
	     (collecting `(setq ,var ,val))) ,@body))

(defmac let (bindings &rest body)
  `(symbol-macrolet
       ,(iter
	 (for bind in bindings)
	 (collecting `(,(first bind)
			,(gensym (strcat (string (first bind)) "_lex")))))
     (let! ,bindings
	   ,@body)))


;;TODO: make into what a normal python user would use!
(defmac or (&rest forms &aux (first-gen (gensym)))
  (if (null (cdr forms))
      (first forms)
      `(let ((,first-gen ,(first forms)))
	 (if ,first-gen
	     ,first-gen
	     (or ,@(cdr forms))))))

(defmac and (&rest forms &aux (first-gen (gensym)))
  (if (null (cdr forms))
      (first forms)
      `(let ((,first-gen ,(first forms)))
	 (if ,first-gen
	     (and ,@(cdr forms))
	     #f))))
(defmac import (&rest args) `(:import ,@args))
(defmac from (&rest args) `(:from ,@args))
(defmac when (test &rest body) `(if ,test (progn ,@body)))
;;todo:add not as unary operator to make translation more pythonic?
(defmac unless (test &rest body) `(if (not ,test) (progn ,@body)))