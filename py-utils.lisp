(in-package :goby)
(defvar *py-hash* (make-hash-table :test #'equalp))

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

(defun emit-form (form)
  (if (atom form)
      (hash-call :atom (list :atom form))
      (hash-call (first form) form)))

(defun emit! (form)
  (with-output-to-string (*py-code*)
    (let ((*indent* "")) (emit-form form))))

(defun emit (form) (w/indent (emit-form form)))

(defmacro w/indent (&rest body)
  `(let ((*indent* (concatenate 'string *indent* *default-indent*))) ,@body))
(defmacro defemit (name args &body body)
  `(setf (gethash ',name *py-hash*)
	 (lambda (form)
	   (let (((_ ,@args)  form))
	     (declare (ignorable _))
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
	  (collecting `(defemit ,name (form1 form2) (out ,(strcat "(~A" (if (listp arg) (second arg) arg) "~A)")
							    (emit! form1) (emit! form2))))))))
							   
(defun out (format-string &rest args)
  (write-string (format nil "~A" *indent*) *py-code*)
  (write-string (apply #'format nil format-string args) *py-code*))

(defun emit-block (args)
  (mapcar #'emit-form args)
  (out "~%"))



(defun py (form)
  (with-output-to-string (*py-code*)
    (emit-form (unroll form))))
