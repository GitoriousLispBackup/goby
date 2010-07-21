(in-package :goby)
;;(if 2 (progn 3 4) 23)
;;(or 2 3 4)

(defvar *false* '|False|)
(defvar *true* '|True|)
(set-dispatch-macro-character
 #\# #\f (lambda (stream char1 char2) `(quote ,*false*)))
(set-dispatch-macro-character
 #\# #\t (lambda (stream char1 char2) `(quote ,*true*)))

;----------------------------------------------------------------------
(defroll atom (value) 
  (condenv value (set! retv value) value))
(defroll setq ((_ var val))
  (let (((:values ret outer) (unroll-arg! val)))
    (condenv
     `(progn ,@outer ,(set! var ret))
     `(progn ,@outer ,(set! var ret) ,(set! retv var))
     (values retv `(,@outer ,(set! var ret) ,(set! retv var))))))

(defunroll macro-unroll (form)
  (unroll (mexpand-all form) :retv retv :in-block in-block))

(defroll progn ((_ &rest arguments))
  (let ((default `(,@(unroll-all! (butlast arguments))
		   ,(unroll-block! (last1 arguments) retv))))
    (condenv `(progn ,@default) `(progn ,@default) (values retv default))))

(defroll if ((_ test then &optional else))
  (let (((:values test-retv test-outer) (unroll-arg! test))
	(unrolled-then (unroll-block! then retv))
	(unrolled-else (unroll-block! else retv))
	(default `(progn ,@test-outer (_if ,test-retv ,unrolled-then ,unrolled-else))))
   (condenv default default (values retv (list default)))))

(defroll function ((name &rest arguments))
  (let (((:values arg-list outer-block) (unroll-args! arguments)))
    (cond
      ((and (null outer-block) (not in-block)) `(,name ,@arg-list))
      (t
       (condenv
	`(progn ,@outer-block (,name ,@arg-list))
	`(progn ,@outer-block ,(set! retv `(,name ,@arg-list)))
	(values retv `(,@outer-block ,(set! retv `(,name ,@arg-list)))))))))

;-------------------------------------------
;def and class
(defroll def ((_ name args &rest body))
  (let ((unrolled-body (unroll-blocks! body)))
    (condenv
     `(_def ,name ,args ,@unrolled-body)
     `(progn (_def ,name ,args ,@unrolled-body) ,(set! retv name))
     (values retv `((_def ,name ,args ,@unrolled-body) ,(set! retv name))))))

(defroll class ((_ name super &rest body))
  (let ((unrolled-body (unroll-blocks! body)))
    (condenv
     `(_class ,name ,super ,@unrolled-body)
     `(progn (_class ,name ,super ,@unrolled-body) ,(set! retv name))
     (values retv `((_class ,name ,super ,@unrolled-body) ,(set! retv name))))))

(defmac while (test &rest body)
  `(progn (_while ,test ,@body) ,*default*))
(defmac for (clauses &rest body)
  `(progn (_for ,clauses ,@body) ,*default*))

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

;--------------------------------------------
;--------------------------------------------
;--------------------------------------------
;--------------------------------------------
(defun special? (form)
  (if (listp form)
      (case (first form)
	((_if _=  _def _while _for _import _from _class) t)
	(t nil))))

(defmacro call-unroll (arg) `(,(symbolicate arg "-unroll") form :in-block in-block :retv retv))
(defunroll unroll (form)
  (cond
    ((special? form) form)
    ((or (atom form) (null form)) (atom-unroll (or form *default*) :in-block in-block :retv retv))
    ((macro? (first form)) (macro-unroll form :in-block in-block :retv retv))
    (t
     (case (first form)
       (setq (call-unroll setq))
       (if (call-unroll if))
       (progn  (call-unroll  progn))
       (def (call-unroll def))
       (class (call-unroll class))
       (t (call-unroll  function))))))
(defunroll top-unroll (form)
  (unroll form :in-block t))

;;TODO: implement macros, symbol-macrolets, macrolet, lexical-let, compiler-let
;;you better make them easy to implement and 'plugin!' (or else!)



