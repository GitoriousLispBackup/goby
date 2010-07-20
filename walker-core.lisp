(in-package :goby)

(defvar *py-walkers* (make-hash-table :test #'equalp))
(defun walker? (name) (gethash (string name) *py-walkers*))
(defun walker! (name fn) (setf (gethash (string name) *py-walkers*) fn))
(defun walker (name) (walker? name))
(defvar *retv* nil)


(defmacro defwalk (name form &body body)
  `(progn  
     (walker!
      ',name (lambda ,form
	       (let ((retv (or *retv* (gensym))))
		 ,@body)))))

(defparameter *return* nil)
(defun block? (val) (subtypep (class-of val) 'block-mixin))
(defmacro defunwalk (name slots &body body)
  `(defmethod unwalk-form! ((self ,name) &key)
     (let ((result-gen (with-slots (retv ,@slots)  self ,@body)))
       result-gen)))

;;TODO: fix macros! parents are not good!
(defwalk macro (&rest mac)
  (walk! (mexpand-all mac) *retv* *use* *env*))


(defun walker-type (form)
  (cond
    ((not (listp form)) 'atom)
    ((null form) 'default)
    (t
     (let ((head (first form)))
       (cond
	 ((macro? head) 'macro)
	 ((function? head) 'function)
	 ((walker? head) head)
	 (t (error "unkown form type ~A" form)))))))

(defparameter *use* nil)

(defvar *parent* *toplevel*)
(defvar *self* *toplevel*)

(defmethod walk! (form &optional  retv use env)
  (let ((type (walker-type form))
	(*env* (or env
		  ; (and (toplevel?) (make-default-env t))
		   (and (not (in-block?)) *env*)
		   (make-default-env)))
	(*parent* *self*)
	(*self* (gensym)))
    (assert type)
    (let ((walker (walker type)))
      (assert (functionp walker))
      (let ((*retv* retv)
	    (*use* use)
	    (result (check-call (modify! (apply walker (mklst form))
					 :id (if (equalp *parent* *toplevel*)
						 (progn (format t "self is ~A" *self*) *self*)
						 *self*)))))
	result))))
(defun walk-all! (forms &optional retv use)
  (mapcar (lambda (x) (walk! x retv use)) forms))

(defun unwalk! (form &optional retv)
  (let ((*return* retv)) (unwalk-form! form)))
(defun unwalk-all! (form &optional retv)
  (mapcar (lambda (x) (unwalk! x retv)) form))

(defmacro use? () `(slot-value self 'use))
(defmacro return? () `(or (use?) *return*))

(defmethod unwalk-form! (form &key)
  (error "no unwalk for form ~A" form))

