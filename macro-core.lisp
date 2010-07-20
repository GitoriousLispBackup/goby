(in-package :goby)
(defvar  *py-macros* (make-hash-table :test #'equalp))
(defun macro? (arg) (gethash arg *py-macros*))
(defun mac-function (arg) (macro? arg))
(defun mac-function! (arg func) (setf (gethash arg *py-macros*) func))
(defmacro defmac (name args &body body)
  `(mac-function! ',name (lambda ,args ,@body)))

(defun function? (arg)
  (and (not (walker? arg))
       (not (macro? arg))))


(defun mexpand (form)
  (cond
    ((atom form) form)
    ((listp form)
     (cond
       ((atom (first form))
	(let ((func (first form)))
	 (cond
	   ((macro? func) (values (apply (mac-function func) (cdr form)) t))
	   (t (values form nil)))))))))

(defun mexpand-all (form)
  (let ((expanded (mexpand form)))
    (if (macro? (and (listp expanded) (first expanded)))
	(mexpand-all expanded)
	expanded)))