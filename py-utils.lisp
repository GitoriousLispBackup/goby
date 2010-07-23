(in-package :goby)
(defvar *py-hash* (make-hash-table :test #'equalp))

(defmacro indent (&rest body)
  `(let ((*indent* (concatenate 'string *indent* *default-indent*))) ,@body))

(defun hash-call (key form)
  (let ((func (gethash key *py-hash*)))
    (if (null func)
	(error "could not find an emit function form form ~A" form)
	(funcall func form))))

(defparameter *py-code* nil)
(defparameter *default-indent* "   ")
(defparameter *indent* "")
(defun arg-list (lst) (format nil "~{~A~^,~}" (mapcar #'emit! lst)))

(defun fresh () (fresh-line *py-code*))

(defun emit (form)
  (cond
    ((atom form)
     (hash-call :atom form))
    ((eql (first form) :function)
     (aif (gethash (second form) *py-hash*)
	  (hash-call (second form) (cddr form))
	  (hash-call :function (cdr form))))
    (t (hash-call (first form) (cdr form)))))

(defun emit! (form)
  (with-output-to-string (*py-code*)
    (let ((*indent* "")) (emit form))))

(defmacro defemit (name args &body body)
  `(setf (gethash ',name *py-hash*)
	 (lambda (form)
	   (let ((,args form))
	    
	     ,@body))))

(defmacro define-multiple-binary (&rest args)
  `(progn ,@(iter
	     (for arg in args)
	     (collecting `(defemit ,arg (&rest args)
			    (out ,(strcat "(~{~A~^" (string arg) "~})")
				 (mapcar #'emit! args)))))))
(defmacro define-single-binary (&rest args)
  `(progn
     ,@(iter
	(for arg in args)
	(let ((name (if (listp arg) (first arg) arg)))
	  (collecting
	   `(defemit ,name
		(form1 form2)
	      (out ,(strcat "(~A" (if (listp arg)
				      (string (second arg)) (string arg)) "~A)")
		   (emit! form1) (emit! form2))))))))
							   
(defun out (format-string &rest args)
  (write-string (format nil "~A" *indent*) *py-code*)
  (write-string (apply #'format nil format-string args) *py-code*))

(defun emit-block (args)
  (mapcar #'emit args)
  (out "~%"))
(defun py (form) (emit! (unroll form)))
