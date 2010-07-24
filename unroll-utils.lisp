(in-package :goby)

(defun constant? (sym) (or (numberp sym) (stringp sym)))
(defvar *default* '|None|)

(defun set! (var val)
  "since we know that val is always a symbol, no need for in-block and retv stuff"
  (if (functionp var)
      (funcall var val)
      `(:= ,(call-unroll 'atom var) ,val)))

(defun unroll-arg! (form &optional in-block) (unroll form :retv (gensym) :in-block in-block))
(defun unroll-block! (form &optional retv) (unroll form :retv retv :in-block t))
(defun unroll-blocks! (forms &optional retv) (mapcar (lambda (x) (unroll-block! x retv)) forms))
(defun unroll-all! (form) (mapcar #'unroll form))

(defvar *symbol-macros* (fset:empty-map))
(defun symbol-macro? (val) (fset:@ *symbol-macros* val))
(defun symbol-macro-function (val) (fset:@ *symbol-macros* val))

(defun symbol? (s) (and (symbolp s) (not (symbol-macro? s))))

;;TODO: fix, make more efficient
(defun unroll-args! (args)
  (if (every (lambda (x) (or (constant? x) (symbol? x))) args)
      args
      (iter
	(for arg in args)
	(let (((:values ret outer) (unroll-arg! arg)))
	  ;;force everything except constants to be on the outer edge!
	  (if (and (not (constant? ret)) (null outer))
	      (let ((gen (gensym)))
		(collecting (set! gen ret) into outer-code)
		(collecting gen into argument-list))
	      (progn
		(appending outer into outer-code)
		(collecting ret into argument-list))))
	(finally (return (values argument-list outer-code))))))

(defvar *unroll-hash* (make-hash-table :test #'equalp))
(defun call-unroll (sym form &key in-block retv)
  (if sym
      (aif (gethash sym *unroll-hash*)
	   (funcall it form in-block retv)
	   (funcall (gethash 'default *unroll-hash*) form in-block retv))))

(defmacro defunroll (name form-bind &rest body)
  (let ((form-gen (gensym)))
    `(setf (gethash ',name *unroll-hash*)
	   (lambda (,form-gen in-block retv)
	     (let ((,form-bind ,form-gen)) ,@body)))))

(defmacro condenv (as-block as-block-and-ret as-embedded)
  `(cond
     ((and in-block retv) ,as-block-and-ret)
     (retv ,as-embedded)
     (t ,as-block)))
