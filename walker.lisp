(in-package :goby)
(defwalk if (test then &optional else)
  (make-instance
   'if
   :test (walk! test)
   :then (modify! (walk! test) :retv retv)
   :else (modify! (walk! else) :retv retv)))

(defunwalk if (test then &optional else)
  `(progn
     ,(block-of test)
     (_if ,(test-of test) ,(then-of test) ,(else-of test))))

;;function
(defun functionize (args)
  "split args into clean and unclean and return such progns!"
  (let (((clean unclean) (split (lambda (x) (and (embeddable? x) (clean? x)) (walk-all! args)))))
    (list (make-clean-progn clean) (make-unclean-progn unclean))))

(defmacro with-side-effects ((clean unclean) args &body body)
  `(let (((,clean ,unclean) (functionize ,args))) ,@body))

(defwalk function (name &rest args)
  (with-side-effects
      (clean unclean) args
      (make-instance
       'function
       :name name
       :clean clean-progn
       :unclean unclean-progn)))

(defunwalk function (name clean unclean)
  `(progn
     ,@(unwalk-all! (unclean-of self))
     (,name ,@(unwalk-all! (clean-of self))
	    ,@(loop for code in (unclean-of self) collecting (return-of code)))))

(defwalk atom (val) (make-instance 'atom :value val))

(defunwalk atom (val) val)

(defwalk and (&rest args)
  (with-side-effects
      (clean unclean) args
      (make-instance
       'and
       :unclean (labels ((make-if-form (retv args)
			   (make-instance
			    'if
			    :retv retv
			    :test (first args)
			    :then (if (single? result)
				      (set! result (true))
				      (make-if-form result (cdr args)))
			    :else (set! result (false)))))
		  (make-if-form (gensym) (body-of unclean-progn)))
       :clean clean-progn)))

(defunwalk and (clean unclean)
  `(progn ,@(unwalk-all unclean) (_and ,@(all-retv-of clean),(retv-of (first unclean)))))







