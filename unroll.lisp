(in-package :goby)

;UTILS
;----------------------------------------------------------------------
(defun prognize (args) (if (length1 args) (first args) `(progn ,@args)))
(defun constant? (sym) (or (numberp sym) (stringp sym)))
(defvar *default* '|None|)
;----------------------------------------------------------------------


(defun progn-unroll (form &key in-block ret)
  (let ((non-return (mapcar #'unroll (butlast (cdr form))))
	(unrolled (unroll (last1 form) :ret ret :in-block t)))
    (cond
      ((and in-block ret) `(progn ,@non-return ,unrolled))
      (ret (values ret `(,@non-return ,unrolled)))
      (t `(progn ,@non-return ,unrolled)))))

(defun function-unroll (form &key in-block ret)
  (iter
    (for arg in (cdr form))
    (if (constant? arg)
	(collecting arg into function-arguments)
	(let (((:values unroll-return unroll-body) (unroll arg :ret (gensym))))
	  (collecting unroll-return into function-arguments)
	  (appending unroll-body into function-outer)))
    (finally (let ((default `(,(first form) ,@function-arguments)))	     
	       (return
		 (cond
		   ((and ret (not in-block)) (values default function-outer))
		   (ret (prognize `(,@function-outer ,(set! ret default))))
		   (t (prognize `(,@function-outer ,default)))))))))

(defun if-unroll (form &key in-block ret)
  (let (((test then &optional else) (cdr form))
	((:values unroll-test outer-test)
	 (unroll test :in-block nil :ret (gensym)))
	(unroll-then (unroll then :in-block t :ret ret))
	(unroll-else (unroll else :in-block t :ret ret))
	(basic (prognize `(,@outer-test
			   (if ,unroll-test ,unroll-then ,unroll-else)))))
    (cond
      ((and in-block ret) basic)
      (ret (values ret (list basic)))
      (t basic))))

(defun atom-unroll (form &key in-block ret)
  (if (and ret in-block)
      (set! ret form)
   form))

(defun unroll (form &key in-block ret)
  (if (and (not (null form)) (listp form))
      (let ((head (first form)))
	(cond
	  ((equalp head 'if)
	   (if-unroll form :in-block in-block :ret ret))
	  ((equalp head 'progn)
	   (progn-unroll form :in-block in-block :ret ret))
	  (t (function-unroll form :in-block in-block :ret ret))))
      (atom-unroll (or form *default*) :in-block in-block :ret ret)))

(defun top-unroll (form &key (in-block t) ret)
  (unroll form
	  :in-block in-block
	  :ret ret))
