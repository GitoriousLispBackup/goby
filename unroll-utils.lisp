(in-package :goby)

(defun constant? (sym) (or (numberp sym) (stringp sym)
			   (equalp sym #f)
			   (equalp sym #t)
			   (equalp sym *default*)))

(defun literal? (obj) (and (listp obj) (equalp (first obj) :literal)))
(defun set! (var val)
  "since we know that val is always a symbol, no need for in-block and retv stuff"
  (if (functionp var)
      (funcall var val)
      (progn
	(assert (or (and (listp var) (equalp (first var) :comma))
		    (symbolp var)))
	`(:= ,(unroll var) ,val))))

(defun unroll-arg! (form &optional in-block)
  (unroll form :retv (gensym) :in-block in-block))

(defun unroll-block! (form &optional retv)
  (if (and (null form) (not retv))
      `(:pass)
      (let ((ret (unroll form :retv retv :in-block t)))
	ret)))

(defun unroll-blocks! (forms &optional retv)
  (if (and (null forms) (not retv))
      `((:pass))
      (mapcar (lambda (x) (unroll-block! x retv)) forms)))

(defun unroll-all! (form) (mapcar #'unroll form))

(defvar *symbol-macros* (fset:empty-map))
(defun symbol-macro? (val) (fset:@ *symbol-macros* val))
(defun symbol-macro-function (val) (fset:@ *symbol-macros* val))

(defun symbol? (s) (and (symbolp s) (not (symbol-macro? s))))




(defun is-function (ret lst)
  (if (and (listp ret) (equalp (first ret) :function))
      (member (second ret) lst)))
(defvar *in-function* nil)
;;TODO: fix, more efficient, and smarter, for example (unroll '(bam (boom))) doesn't do too well
(defun pythonify-args (args)
  (iter (for arg in args)
	(if (symbolp arg)
	    (collecting (pythonify arg))
	    (collecting arg))))
(defun unroll-args! (args)
  (iter
    (for arg in args)
    (let (((:values ret outer) (let ((*in-function* t))  (unroll-arg! arg))))
      ;;force everything except constants and certain in-built functions to be on the outer edge!
      ;;also exclude 'literals'
      (if (and (not (literal? ret)) (not (constant? ret)) (null outer) (not (is-function ret '(+ - * / ** << >> % str int float not))))
	  (let ((gen (gensym)))
	    (collecting (set! gen ret) into outer-code)
	    (collecting gen into argument-list))
	  (progn
	    (appending outer into outer-code)
	    (collecting ret into argument-list))))
    (finally (return (values argument-list outer-code)))))

(defvar *unroll-hash* (make-hash-table :test #'equalp))
(defun call-unroll (sym form &key in-block retv)
  (if  sym
   (aif (gethash sym *unroll-hash*)
	(funcall it form in-block retv)
	(funcall (gethash 'default *unroll-hash*) form in-block retv))
   form))
(defun lower-case-symbol (nm)
  (read-from-string (strcat "|" (string-downcase (string nm)) "|")))
(defmacro defunroll (name form-bind &rest body)
  (let ((form-gen (gensym)))
    `(setf (gethash ',name *unroll-hash*)
	   (lambda (,form-gen in-block retv)
	     (let ((,form-bind ,form-gen)) ,@body))

	   (gethash ',(lower-case-symbol name) *unroll-hash*)
	   (lambda (,form-gen in-block retv)
	     (let ((,form-bind ,form-gen)) ,@body)))))

(defmacro condenv (as-block as-block-and-ret as-embedded)
  `(cond
     ((and in-block retv) ,as-block-and-ret)
     (retv ,as-embedded)
     (t ,as-block)))
