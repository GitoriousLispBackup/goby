;;todo: why does (unroll '(bam (boom))) (py '(if (or 2 3) 3))
(in-package :goby)
;;todo : can we quicken things by getting rid of all hash look up per type like we had before
;;(if 2 (progn 3 4) 23)
;;(or 2 3 4)
;;TODO: try statement, with statement, in, read manual please!
;----------------------------------------------------------------------

(defunroll atom value
  (if (symbol-macro? value)
      (unroll (symbol-macro-function value))
      (condenv value (set! retv value) value)))

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
  (unroll (mexpand-all form) :retv retv :in-block in-block))

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
	(values retv `(,@outer-block ,(set! retv `(:function ,name ,@arg-list)))))))))

;-------------------------------------------
;def and class
(defunroll def ((_ name args &rest body))
  (let ((unrolled-body (unroll-blocks!
			body
			(lambda (arg) `(:function |return| ,arg)))))
    (condenv
     `(:def ,name ,args ,@unrolled-body)
     `(:progn (:def ,name ,args ,@unrolled-body) ,(set! retv name))
     (values retv `((:def ,name ,args ,@unrolled-body) ,(set! retv name))))))

(defunroll class ((_ name super &rest body))
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



