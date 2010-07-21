(in-package :goby)
;----------------------------------------------------------------------
(defun constant? (sym) (or (numberp sym) (stringp sym)))
(defvar *default* '|None|)

(defun set! (var val) `(_= ,var ,val))
(defun unroll-arg! (form) (unroll form :retv (gensym)))
(defun unroll-block! (form &optional retv) (unroll form :retv retv :in-block t))
(defun unroll-all! (form) (mapcar #'unroll form))
(defun unroll-args! (arguments)
  (iter (for arg in arguments)
        (if (constant? arg)
	    (collecting arg into argument-list)
	    (let (((ret outer) (unroll-arg arg)))
	      (collecting ret into argument-list)
	      (appending outer into outer-code)))
	(finally (return (values argument-list outer-code)))))

(defmacro defunroll (name arg &rest body) `(defun ,name (,@arg &key retv in-block) ,@body))
(defmacro defroll (name form-bind &rest body)
  (let ((form-gen (gensym)) (new-name (symbolicate name "-unroll")))
    `(defunroll ,new-name (,form-gen)
       (let ((,form-bind ,form-gen))
	 ,@body))))
(defmacro condenv (as-block as-block-and-ret as-embedded)
  `(cond
     ((and in-block retv) ,as-block-and-ret)
     (retv ,as-embedded)
     (t ,as-block)))

(defmacro call-unroll (arg) `(,(symbolicate arg "-urnoll") :in-block in-block :retv retv))
(defunroll unroll (form)
  (cond
    ((or (atom form) (null form)) (atom-unroll (or form *default*) :in-block in-block :retv retv))
    ((listp form)
     (case (first form)
       (if (call-unroll  if))
       (progn (call-unroll  progn) ;(progn-unroll form :in-block in-block :retv retv)
	       )
       (t (call-unroll  function))))
    (t (error "what are you?~A" form))))
(defunroll top-unroll (form)
  (unroll! form))
;----------------------------------------------------------------------

(defroll atom (val)
  (condenv atom (set! retv atom) atom))

(defroll progn (_ &rest arguments)
  (let ((defualt `(progn ,@(unroll-all! (butlast arguments))
			 ,@(unroll-block! (last1 arguments) retv))))
    (condenv default default (values retv default))))

(defroll function (name &rest arguments)
  (let (((arg-list outer-block) (unroll-args! arguments)))
   (condenv
    `(progn ,@outer-block (,name ,@arg-list))
    `(progn ,@outer-block ,(set! retv '(,name ,@arg-list)))
    (values retv `(progn ,@outer-block ,(set! retv '(,name ,@arg-list)))))))

(defroll if (_ test then &optional else)
  (let (((test-retv test-outer) (unroll-arg! test))
	(unrolled-then (unroll-block! then))
	(unrolled-else (unroll-block! else))
	(default (progn ,@test-outer (if ,test-retv ,unrolled-then ,unrolled-else))))
   (condenv default default (values retv default))))

;-------------------------------------------
;def and class
(defroll def (_ name args &rest body)
  (let ((unrolled-body (unroll-blocks! body)))
    (condenv
     `(def ,name ,args ,@unrolled-body)
     `(progn (def ,name ,args ,@unrolled-body) ,(set! retv name))
     (values name `((def ,name ,args ,@unrolled-body))))))

(defroll class (_ name super &rest body)
  (let ((unrolled-body (unroll-blocks! body)))
    (condenv
     `(class ,name ,super ,@unrolled-body)
     `(progn (class ,name ,super ,@unrolled-body) ,(set! retv name))
     (values name `((class ,name ,super ,@unrolled-body))))))
;--------------------------------------------


;;TODO: implement macros, symbol-macrolets, macrolet, lexical-let, compiler-let
;;you better make them easy to implement and 'plugin!' (or else!)



