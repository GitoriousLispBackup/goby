(in-package :goby)
(defparameter *py-walkers* (make-hash-table :test #'equalp))
(defun walker? (name) (gethash (string name) *py-walkers*))
(defun walker! (name fn) (setf (gethash (string name) *py-walkers*) fn))
(defun walker (name) (walker? name))
(defmacro defwalk (name form &body body)
  `(progn
     (add-walker ',name (lambda ,form
			  (let ((retv (gensym)))
			    ,@body)))))
(defun walker-type (form)
  (let ((head (first form)))
    (cond
      ((macro? head) 'macro)
      ((function? head) 'function)
      ((atom head) 'atom)
      ((walker? head) head)
      (t (error "unkown form type ~A" form)))))

(defun walk! (form)
  (apply (walker (walker-type form)) form))
