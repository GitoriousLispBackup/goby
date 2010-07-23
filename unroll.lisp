;;todo: why does (unroll '(bam (boom))) (py '(if (or 2 3) 3))
(in-package :goby)
;;(if 2 (progn 3 4) 23)
;;(or 2 3 4)
;;TODO: try statement, with statement, in, read manual please!
;----------------------------------------------------------------------
(defroll atom (value) 
  (condenv value (set! retv value) value))

(defroll setq ((_ var val))
  (let (((:values ret outer) (unroll-arg! val)))
    (condenv
     `(:progn ,@outer ,(set! var ret))
     `(:progn ,@outer ,(set! var ret) ,(set! retv var))
     (values retv `(,@outer ,(set! var ret) ,(set! retv var))))))

(defunroll macro-unroll (form)
  (unroll (mexpand-all form) :retv retv :in-block in-block))

(defroll progn ((_ &rest arguments))
  (let ((default `(,@(unroll-all! (butlast arguments))
		   ,(unroll-block! (last1 arguments) retv))))
    (condenv `(:progn ,@default) `(:progn ,@default) (values retv default))))

(defroll if ((_ test then &optional else))
  (let (((:values test-retv test-outer) (unroll-arg! test))
	(unrolled-then (unroll-block! then retv))
	(unrolled-else (unroll-block! else retv))
	(default `(:progn ,@test-outer (:if ,test-retv ,unrolled-then ,unrolled-else))))
   (condenv default default (values retv (list default)))))

(defroll function ((name &rest arguments))
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
(defroll def ((_ name args &rest body))
  (let ((unrolled-body (unroll-blocks!
			body
			(lambda (arg) `(:function |return| ,arg)))))
    (condenv
     `(:def ,name ,args ,@unrolled-body)
     `(:progn (:def ,name ,args ,@unrolled-body) ,(set! retv name))
     (values retv `((:def ,name ,args ,@unrolled-body) ,(set! retv name))))))

(defroll class ((_ name super &rest body))
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
  (if (listp form)
      (keywordp (first form))))

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

;;TODO: symbol-macrolets, macrolet, lexical-let, compiler-let
;;you better make them easy to implement and 'plugin!' (or else!)



