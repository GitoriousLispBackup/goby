;;todo: why does (unroll '(bam (boom))) (py '(if (or 2 3) 3))
(in-package :goby)
;;todo : can we quicken things by getting rid of all hash look up per type like we had before
;----------------------------------------------------------------------

;;todo: literal shouldn't be expose to user code?
(defunroll literal (_ &rest args)
  (let (((:values ret outer) (unroll-args! args)))
    (values `(:literal ,@ret) outer)))

(defmac @ (&rest args)
  `(literal
    ,@(butlast
       (iter
	 (for arg in args)
	 (appending (list arg ":"))))))

(defunroll ref (_ var &rest where)
  (let (((:values ret outer-code) (unroll var :retv (gensym)))
	((:values ret2 outer-code2) (unroll-args! where)))
    (condenv
     `(:progn ,@outer-code ,@outer-code2 (:ref ,ret ,ret2))
     `(:progn ,@outer-code ,@outer-code2 ,(set! retv `(:ref ,ret ,ret2)))
     (values retv `(,@outer-code
		    ,@outer-code2 ,(set! retv `(:ref ,ret ,ret2)))))))

;(a.b.c.d)

(defunroll atom value
  (cond
    ((null value) (unroll *default* :in-block in-block :retv retv))
    (t (cond	   
	 ((symbol-macro? value)
	  (unroll (symbol-macro-function value) :in-block in-block :retv retv))
	 (t (condenv value (set! retv value) value))))))


(defunroll symbol-macrolet (_ bindings &rest body)
  (declare (ignorable _))
  
  (let ((*symbol-macros* (with-all *symbol-macros* bindings)))
    (unroll `(progn ,@body) :retv retv :in-block in-block)))

(defunroll setq (_ var val)
  (declare (ignorable _))
  (let (((:values ret outer) (unroll-arg! val)))
    (condenv
     `(:progn ,@outer ,(set! var ret))
     `(:progn ,@outer ,(set! var ret) ,(set! retv var))
     (values retv `(,@outer ,(set! var ret) ,(set! retv var))))))

(defunroll macro form
  (unroll (mexpand-all form :retv retv :in-block in-block) :retv retv :in-block in-block))

(defunroll progn (_ &rest arguments)
  (declare (ignorable _))
  (let ((default `(,@(unroll-all! (butlast arguments))
		   ,(unroll-block! (last1 arguments) retv))))
    (condenv `(:progn ,@default) `(:progn ,@default) (values retv default))))

(defunroll if (_ test then &optional else)
  (declare (ignorable _))
  (let (((:values test-retv test-outer) (unroll-arg! test))
	(unrolled-then (unroll-block! then retv))
	(unrolled-else (unroll-block! else retv))
	(default `(:progn ,@test-outer (:if ,test-retv ,unrolled-then ,unrolled-else))))
   (condenv default default (values retv (list default)))))

(defunroll default (name &rest arguments)
  (assert (symbolp name) nil "in function, ~A is not a valid function name" name)
  (let (((:values arg-list outer-block) (unroll-args! arguments)))
    (cond
      ((and (null outer-block) (not in-block)) `(:function ,name ,@arg-list))
      (t
       (condenv
	`(:progn ,@outer-block (:function ,name ,@arg-list))
	`(:progn ,@outer-block ,(set! retv `(:function ,name ,@arg-list)))
	(values retv `(,@outer-block
		       ,(set! retv `(:function ,name ,@arg-list)))))))))

;;TODO: implicit progn
(defunroll try (_ body &rest clauses)
  (let ((ret (unroll-block! body retv))
	(expanded-clauses
	 (iter
	   (for clause in clauses)
	   (assert (listp clause))
	   (if (keywordp (first clause))
	       (collecting `(,(first clause)
			      ,(unroll-block! `(progn ,@(cdr clause)))))
	       (collecting `(:except (,(first clause) ,(second clause))
				     ,(unroll-block!
					`(progn ,@(cddr clause)))))))))
    (condenv
     `(:try ,ret ,expanded-clauses)
     `(progn  ,(set! retv *default*) (:try ,ret ,expanded-clauses))
     (values retv `(,(set! retv *default*) (:try ,ret ,expanded-clauses))))))

;-------------------------------------------
;def and class
(defunroll break (_ &optional ret)
  (condenv
   `(:break)
   `(:progn ,(set! retv ret) (:break))
   (values retv `(,(set! retv ret) (:break)))))

(defunroll continue (_)
  (condenv
   `(:continue) `(:continue)
   (values nil `(:continue))))

(defunroll def (_ name args &rest body)
  (let ((unrolled-body (unroll-blocks!
			body
			(lambda (arg) `(:function |return| ,arg)))))
    (condenv
     `(:def ,name ,args ,@unrolled-body)
     `(:progn (:def ,name ,args ,@unrolled-body) ,(set! retv name))
     (values retv `((:def ,name ,args ,@unrolled-body) ,(set! retv name))))))

(defunroll class (_ name super &rest body)
  (let ((unrolled-body (unroll-blocks! body)))
    (condenv
     `(:class ,name ,super ,@unrolled-body)
     `(:progn (:class ,name ,super ,@unrolled-body)
	      ,(set! retv name))
     (values retv `((:class ,name ,super ,@unrolled-body)
		    ,(set! retv name))))))



;--------------------------------------------
;--------------------------------------------
;--------------------------------------------
;--------------------------------------------
(defun special? (form)
  (and (listp form) (keywordp (first form))))

(defun unroll (form &key retv in-block)
  (call-unroll
   (cond
     ((special? form) nil)
     ((or (atom form) (null form)) 'atom)
     ((macro? (first form)) 'macro)
     (t (first form)))
   form :retv retv :in-block in-block))

(defun top-unroll (form)
  (unroll form :in-block t))

;;TODO: symbol-macrolets, macrolet, lexical-let, compiler-let
;;you better make them easy to implement and 'plugin!' (or else!)



