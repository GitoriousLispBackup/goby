(in-package :goby)
(defvar  *py-macros* (make-hash-table :test #'equalp))
(defun macro? (arg) (gethash arg *py-macros*))
(defun mac-function (arg) (macro? arg))
(defun mac-function! (arg func) (setf (gethash arg *py-macros*) func))
(defmacro defmac (name args &body body)
  `(progn
     (mac-function! ',name (lambda (retv in-block ,@args) ,@body)) 
     (mac-function! ',(read-from-string (strcat "|" (string-downcase (string name)) "|")) (lambda (retv in-block ,@args) ,@body))
     (export ',(read-from-string (strcat "|" (string-downcase (string name)) "|")) :goby)
)
)

(defun mexpand (form &key retv in-block)
  (cond
    ((atom form) form)
    ((listp form)
     (cond
       ((atom (first form))
	(let ((func (first form)))
	  (cond
	    ((macro? func) (values (apply (mac-function func)
					  retv in-block (cdr form)
					  ) t))
	    (t (values form nil)))))))))

(defun mexpand-all (form &key retv in-block)
  (let ((expanded (mexpand form :retv retv :in-block in-block)))
    (if (macro? (and (listp expanded) (first expanded)))
	(mexpand-all expanded :retv retv :in-block in-block)
	expanded)))
