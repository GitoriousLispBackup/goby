(defpackage :py (:use :cl))
(defmethod str ((str string)) str)
(defmethod str ((sym symbol)) (string sym))
(defmethod str (obj) (write-to-string obj))
(defun strcat (&rest args) (apply #'concatenate 'string args))
(defun functionize (form)
  (loop
     for arg in (cdr form)
     collecting (mexpand-all arg) into result
     finally (return `(,(first form) ,@result))))
;;;;;;;;;;;;;;;;;;;;;
(defparameter *py-macros* (make-hash-table :test #'equalp))
(defun macro? (arg) (gethash arg *py-macros*))
(defun mac-function (arg) (macro? arg))

(defun mac-function! (arg func) (setf (gethash arg *py-macros*) func))
(defmacro defmac (name args &body body)
  `(mac-function! ',name (lambda ,args ,@body)))

(defparameter *py-special* (make-hash-table :test #'equalp))
(defun special? (arg) (gethash arg *py-special*))
(defun special-function? (arg) (not (special? arg)))
(defun special-function (arg) (special? arg))
(defun special-function! (arg func) (setf (gethash arg *py-special*) func))
(defmacro defspecial (name args &body body)
  `(special-function! ',name (lambda ,args ,@body)))
(defun function? (arg) (and (not (special? arg)) (not (macro? arg))))

(defun expand-special (form)
  (functionize form))

(defun mexpand (form)
  (cond
    ((atom form) form)
    ((listp form)
     (cond
       ((atom (first form))
	(let ((func (first form)))
	 (cond
	   ((macro? func) (apply (mac-function func) (cdr form)))
	   (t form))))))))


(defun mexpand-all (form)
  (let ((expanded (mexpand form)))
    (cond
      ((atom expanded) expanded)
      ((macro? (first expanded)) (mexpand-all expanded))
      ((function? (first expanded)) (functionize expanded))
      ((special? (first expanded)) (expand-special expanded))
      (t (error "um... ~A" expanded)))))
(defun mexpand! (form)
  (mexpand-all form))

;(mexpand! '(let ((a 2)) (pr (let ((a 3)) 2))))
(defun prognize (form)
  (loop
       for arg in form
       collecting (mexpand-all arg) into result
       finally (return (format nil "~{~A~%~}" result))))
(defspecial = (variable value) (format nil "~A = ~S" variable value))
(defspecial progn (&rest args) (prognize args))
(defspecial print (arg) (format nil "print ~A" arg))
(defmac let (bindings &rest body)
  `(progn
     ,@(loop for (variable value) in bindings
	  collecting `(= ,variable ,value))
     ,@body))

;;todo: when expanding, get the return form also (somehow)!
(defun parse-function-arg (form argument-return-form ret)
  (cond
    (ret (values arg ret))
    ((function? arg) (values arg nil))
    (t (values arg ret))))

(let (((:values form not-as-ret (parse-function-arg (mexpand-all form)))))
  (cond
    (not-as-ret
     (strcatf code-block form)
     (strcatf args not-as-ret))
    (t
     (strcatf args not-as-ret))))