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
(defmacro defunwalk (name slots &body body)
  `(defmethod unwalk-form! ((self ,name) &key)
     (let ((result-gen (with-slots (retv ,@slots)  self ,@body)))
       (if (slot-value self 'use) `(progn ,(set-default! (retv-of self)) ,result-gen) result-gen))))

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


(defmethod walk! (form &optional retv)
  (let ((type (walker-type form)))
    (assert type)
    (let ((walker (walker type)))
      (assert (functionp walker))
      (let ((*retv* retv)) (apply walker (mklst form))))))

(defun unwalk! (form &key) (let ((*return* nil)) (unwalk-form! form)))
(defun unwalk-retv! (form &optional retv)
  (let ((*return* t)) (unwalk-form! (if retv (modify! form :retv retv) form))))

(defun unwalk-all! (form &key)
  (mapcar (lambda (x) (unwalk! x)) form))
(defun unwalk-retv-all! (form &optional retv)
  (mapcar (lambda (x) (unwalk-retv! x retv)) form))
(defmacro return? () `(or (slot-value self 'use) *return*))



(defmethod unwalk-form! (form &key)
  (error "no unwalk for form ~A" form))

