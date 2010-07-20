(in-package :goby)
(defparameter *env* (make-hash-table :test #'equalp))
(defun get-env (key) (gethash key *env*))

(defmacro w/env! ((&key enclosing-type) &body body)
  `(let ((new-hash (make-hash-table :test #'equalp))
	 (*env* (progn (setf (gethash :enclosing-type new-hash) ,(or enclosing-type (get-env :enclosing-type)))
		       new-hash)))
     ,@body))

(defmacro w/block (&body body) `(w/env! (:enclosing-type :block) ,@body))
(defun in-block? ()
  (equalp (get-env :enclosing-type) :block))

(defun call-parent (func)
  (push  (list func *parent*) (gethash :parent-calls *env*)))

;;todo: make more efficient!
(defun check-call (self)  
  (iter
    (for (func parent-id) in (get-env :parent-calls))
    (when (equalp (id-of self) parent-id)
      (assert (functionp func))
      (for result = (funcall func self))
      (finally  (return (or result self))))))
