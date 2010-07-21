(in-package :goby)
;----------------------------------------------------------------------
(defroll atom (value) 
  (condenv value (set! retv value) value))

(defroll progn ((_ &rest arguments))
  (let ((default `(progn ,@(unroll-all! (butlast arguments))
			 ,(unroll-block! (last1 arguments) retv))))
    (condenv default default (values retv default))))

(defroll if ((_ test then &optional else))
  (let (((:values test-retv test-outer) (unroll-arg! test))
	(unrolled-then (unroll-block! then retv))
	(unrolled-else (unroll-block! else retv))
	(default `(progn ,@test-outer (if ,test-retv ,unrolled-then ,unrolled-else))))
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
     `(def ,name ,args ,@unrolled-body)
     `(progn (def ,name ,args ,@unrolled-body) ,(set! retv name))
     (values retv `((def ,name ,args ,@unrolled-body) ,(set! retv name))))))

(defroll class ((_ name super &rest body))
  (let ((unrolled-body (unroll-blocks! body)))
    (condenv
     `(class ,name ,super ,@unrolled-body)
     `(progn (class ,name ,super ,@unrolled-body) ,(set! retv name))
     (values retv `((class ,name ,super ,@unrolled-body) ,(set! retv name))))))
;--------------------------------------------

(defmacro call-unroll (arg) `(,(symbolicate arg "-unroll") form :in-block in-block :retv retv))
(defunroll unroll (form)
  (cond
    ((or (atom form) (null form)) (atom-unroll (or form *default*) :in-block in-block :retv retv))
    ((listp form)
     (case (first form)
       (if (call-unroll  if))
       (progn (call-unroll  progn))
       (def (call-unroll def))
       (class (call-unroll class))
       (t (call-unroll  function))))))
(defunroll top-unroll (form)
  (unroll form :in-block t))
;;TODO: implement macros, symbol-macrolets, macrolet, lexical-let, compiler-let
;;you better make them easy to implement and 'plugin!' (or else!)



