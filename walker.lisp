;ex1 (walk! '(if (if (if 2 3) (if 2 3) 4)  5))
;ex2 (walk! '(or (if (not (gaush)) (print "HI") (print "LOW")) "NO!!!!!" "YES!!!"))
;;ex3 (walk! '(or2 (if 2 3 4) 3 5))

(in-package :goby)
(defclass walker ()
  ((id :initarg :id :initform (gensym) :reader id-of)
   (retv :initarg :retv :initform (or *retv* (gensym)))
   (use :initarg :use :initform *use* :accessor use-of)))
(defclass constant-form (walker) ((value :initarg :value)))
(defmacro new-form (class &rest slots)
  `(make-instance ,class :retv retv ,@slots))
(defclass block-mixin () ())

(defun embeddable? (val) (not (block? val)))

(defvar *default* '|None|)

;;setq-form
(defclass setq-form (block-mixin walker)
  ((variable :initarg :variable)
   (value :initarg :value
	  :initform (walk! *default*))))

(defun retv-if! (obj)
  (if (embeddable? obj) obj
       (progn
	 (modify! obj :use t)
	 (retv! obj))))
(defwalk setq (_ var val)
  (let ((walk-var (walk! var))
	(walk-val (walk! val))
	(setq-form (new-form 'setq-form
			      :variable walk-var
			      :value (retv-if! walk-val))))
    (if (embeddable? walk-val)
	setq-form
	(new-form 'implicit-progn
		  :body (list walk-val setq-form)))))

(defunwalk setq-form (variable value)
  (let ((default (set! (unwalk! variable) (unwalk! value))))
    (if (return?)
	`(progn ,default ,(set! retv (unwalk! variable)))
	default)))

;;;;;


(defun set! (a b)
  `(_= ,a ,b))
(defun set-default! (a) `(_= ,a ,*default*))
;;variable
(defclass var (constant-form) ((name :accessor name-of :initarg :name)))

(defunwalk var (name) (if (return?) (set! retv name) name))
(defun var! (name) (make-instance 'var :name name))
(defun retv! (obj) (var! (slot-value obj 'retv)))
(defun map-retv! (args) (mapcar #'retv! args))
;;default
(defclass default (walker) ((value :initform *default*)))
(defwalk default () (make-instance 'default))
(defunwalk default (value)
  (if (return?) (set! retv value) value))
;;implicit-progn
(defclass implicit-progn (block-mixin walker)
  ((body :initarg :body :accessor body-of)))
(defwalk progn (_ &rest body)
  (make-instance 'implicit-progn :body (walk-all! body)))
(defun reduce-progn (form)
  (if (equalp (first form) 'progn)
      (iter
	(for arg in form)
	(appending (if (and (listp arg) (equalp (first arg) 'progn))
			(cdr arg)
			(list arg))))
      form))

;;HIGH TODO: reduce!, if the second to last value is embeddable and is used
;;in the last1 one time, then embed it!
(defunwalk implicit-progn (body)
  (reduce-progn
   (if (return?)
       `(progn ,@(unwalk-all! (butlast body))
	       ,(unwalk! (last1 body) retv))
       `(progn ,@(unwalk-all! body)))))
;;atom-form

(defclass atom-form (constant-form) ())

(defwalk atom (val) (make-instance 'atom-form :value val))
(defunwalk atom-form (value) (if (return?) `(_= ,retv ,value) value))
;;if-form
;;TODO: reduce!, for tertiary if!
(defclass if-form (block-mixin walker)
  ((test :initarg :test) (then :initarg :then) (else :initarg :else)))
(defwalk if (_ test then &optional else)
  (let ((w-test (walk! test))     
	(if-form
	 (new-form
	  'if-form
	  :test (if (not (embeddable? w-test)) (retv! w-test) w-test)
	  :then (w/block (walk! then retv))
	  :else (w/block (walk! else retv)))))
    (if (not (embeddable? w-test))
	(new-form
	 'implicit-progn
	 :body (list (modify! w-test :use t) if-form))
	if-form)))

(defunwalk if-form (test then else retv)
  (if (return?)
      `(_if ,(unwalk! test) ,(unwalk! then retv) ,(unwalk! else retv))
      `(_if  ,(unwalk! test) ,(unwalk! then) ,(unwalk! else))))

;;function-form
(defclass function-form (walker)
  ((name :initarg :name) (arguments :initarg :arguments :initform nil)))

(defun clean? (a)
  (subtypep (class-of a) 'constant-form))
(defun dirty? (a) (not (clean? a)))
(defun split (lst by)
  (iter
    (for the-lst initially lst then (cdr the-lst)) (until (null the-lst))
    (collecting (first the-lst) into result)
    (if (funcall by (first the-lst))
	(leave (list (butlast result) the-lst)))
    (finally (return (list lst nil)))))

;;TODO: if embedded things other th
;;some cases here
#|
split at first 'embeddable' which is not a constant
every other 'embeddable' which is not a constant will have to be
non-embeddable! yeah!

|#
(defun embeddable-list (args &aux (walked-args
				   (mapcar #'walk! args)))
  (if (every #'embeddable? walked-args)
      (list walked-args nil)
      (iter
	(for walked-arg in walked-args)
	(let ((embed-it (clean? walked-arg)))
	  
	  (if embed-it
	      (collecting walked-arg into embedded)
	      (progn
		(modify! walked-arg :use t)
		(collecting walked-arg into not-embedded)
		(collecting (retv! walked-arg) into embedded))))
	(finally (return (list embedded not-embedded))))))

(defun prognize (parent retv body)
  ;;look at return value
  (new-form 'implicit-progn :body (append body (list parent))))

(defwalk function (name &rest args)
  (let (((embedded not-embedded) (embeddable-list args))
	(function-form (new-form 'function-form :name name :arguments embedded)))
    (cond
      ((and not-embedded (not (in-block?)))
       (prog1 function-form
	 (call-parent
	  (lambda (parent)
		  (prognize parent retv not-embedded)))))
      (not-embedded
       (new-form 'implicit-progn :body (list not-embedded function-form)))
      (t function-form))))

(defunwalk function-form (arguments name)
  (let ((unwalked `(,name ,@(unwalk-all! arguments))))
    (if (return?)
	(set! retv unwalked)
	unwalked)))

;;todo 'bitwise or'
#|(defbinary + - * / % **
	   ^ % << >>
	   = <= >= < >)|#
;;and-form or-form
(defclass and-form (walker) ((body :initarg :body :initform nil)))
 (defclass or-form (walker) ((body :initarg :body :initform nil)))
;;;never embed and, unless all arguments are embeddable!
(defmac and2 (&rest args)
  (if (null (cdr args))
      (first args)
      `(if ,(first args)
	   (and2 ,@(cdr args)))))
(defmac or2 (&rest args)
  (if (null (cdr args))
      (first args)
      `(if (not ,(first args))
	   'False
	   (or2 ,@(cdr args)))))

(defwalk and (_ &rest args)
     (if args
	 (let (((embedded not-embedded) (embeddable-list args))
	       (and-form (new-form 'and-form :body embedded)))
	   (if (and (> (length embedded) 1) (not not-embedded))
	       and-form
	       (walk! `(and2 ,@args) *retv* *use*)))))
(defunwalk and-form (body)
  `(_and ,@(unwalk-all! body)))

(defwalk or (_ &rest args)
     (if args
	 (let (((embedded not-embedded) (embeddable-list args))
	       (or-form (new-form 'or-form :body embedded)))
	   (if (and (> (length embedded) 2) (not not-embedded))
	       or-form
	       (walk! `(or2 ,@args) *retv* *use*)))))
(defunwalk or-form (body)
  `(_or ,@(unwalk-all! body)))

