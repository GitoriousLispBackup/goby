(in-package :goby)

(defun constant? (sym) (or (numberp sym) (stringp sym)))
(defvar *default* '|None|)
(defun set! (var val) `(:= ,var ,val))
(defun unroll-arg! (form &optional in-block) (unroll form :retv (gensym) :in-block in-block))
(defun unroll-block! (form &optional retv) (unroll form :retv retv :in-block t))
(defun unroll-blocks! (forms &optional retv) (mapcar (lambda (x) (unroll-block! x retv)) forms))
(defun unroll-all! (form) (mapcar #'unroll form))

(defun symbol? (s) (symbolp s))
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

(defmacro defunroll (name arg &rest body) `(defun ,name (,@arg &key retv in-block) ,@body))
(defmacro defroll (name form-bind &rest body)
  (let ((form-gen (gensym)) (new-name (symbolicate name "-unroll")))
    `(defunroll ,new-name (,form-gen)
       (let ((,@form-bind ,form-gen))
	 ,@body))))
(defmacro condenv (as-block as-block-and-ret as-embedded)
  `(cond
     ((and in-block retv) ,as-block-and-ret)
     (retv ,as-embedded)
     (t ,as-block)))
