;(walk! '(if (if (if 2 3) (if 2 3) 4)  5))
(in-package :goby)
(defclass walker ()
  ((retv :initarg :retv :initform (or *retv* (gensym)) :accessor retv-of)
   (use :initarg :use :initform nil :accessor use?)))
(defmacro new-form (class &rest slots)
  `(make-instance ,class :retv retv ,@slots))
(defclass conditional-mixin () ())
(defun conditional? (val) (subtypep (class-of val) 'conditional-mixin))
(defun block? (val) (subtypep  (class-of val) 'implicit-progn))
(defun embeddable? (val) (not (or (conditional? val) (block? val))))

(defvar *default* '|None|)
(defun set! (a b) `(_= ,a ,b))
(defun set-default! (a) `(_= ,a ,*default*))
;;variable
(defclass var (walker) ((name :accessor name-of :initarg :name)))
(defunwalk var (name) (if (return?) (set! retv self) name))
(defun var! (name) (make-instance 'var :name name))
;;default
(defclass default (walker) ((value :initform *default*)))
(defwalk default () (make-instance 'default))
(defunwalk default (value)
  (if (return?) (set! retv value) value))
;;implicit-progn
(defclass implicit-progn (walker) ((body :initarg :body :accessor body-of)))
(defwalk progn (_ body) (make-instance 'implicit-progn :body body))
(defun reduce-progn (form)
  (if (equalp (first form) 'progn)
      (iter
	(for arg in form)
	(appending (if (and (listp arg) (equalp (first arg) 'progn))
			(cdr arg)
			(list arg))))
      form))

(defunwalk implicit-progn (body)
  (reduce-progn
   (if (return?)
       `(progn ,@(unwalk-all! (butlast body))
	       ,(unwalk-retv! (last1 body) retv))
       `(progn ,@(unwalk-all! body)))))
;;atom-form
(defclass atom-form (walker) ((value :initarg :value)))
(defwalk atom (val) (make-instance 'atom-form :value val))
(defunwalk atom-form (value) (if (return?) `(_= ,retv ,value) value))
;;if-form
(defclass if-form (conditional-mixin walker) ((test :initarg :test) (then :initarg :then) (else :initarg :else)))
(defwalk if (_ test then &optional else)
  (let ((w-test (walk! test))     
	(if-form
	 (new-form
	  'if-form
	  :test (if (not (embeddable? w-test)) (var! (retv-of w-test)) w-test)
	  :then (walk! then retv)
	  :else (walk! else retv))))
    (if (not (embeddable? w-test))
	(new-form
	 'implicit-progn
	 :body (list (modify! w-test :use t) if-form))
	if-form)))

(defunwalk if-form (test then else)
  (if (return?)
      `(_if ,(unwalk! test) ,(unwalk-retv! then) ,(unwalk-retv! else))
      `(_if ,(unwalk! test) ,(unwalk! then) ,(unwalk! else))))


#|
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







|#