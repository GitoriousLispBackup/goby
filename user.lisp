(in-package :goby)
(defvar repl nil)
(defun repl ()
  (if repl (close repl))
  (let ((socket (make-socket)))
    (connect
     socket
     (lookup-hostname "localhost")
     :port 5556)
    (setf repl socket)))
(defun send (&rest args) (apply #'format repl args) (finish-output repl))

(defun pypy (st)
  (let ((*readtable* (named-readtables:find-readtable :modern)))
    (py
     (read-from-string
      (format nil
	      "(progn (setq toplevel (progn ~A))
                         (print (+ (str toplevel)
                                   \"\\\\n\")))
" st )))))

;;todo: dispatch macros, lowercase, and nil, and print/exec/
(defun pyval (st)
  (if (socket-connected-p repl)
      (send (pypy st))
      (format t "pypy: socket not connected!~%")))