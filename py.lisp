(in-package :goby)
;;TODO: for some odd, weird reason, py takes *forever*!!! fix eventually!
;-------------------------------------------------
(defemit :continue ()
  (fresh) (out "continue~A"))

(defemit :while (test body)
  (fresh)
  (out "while ~A:~%" test)
  (indent (emit body)))
(defemit :pass ()
  (fresh)
  (out "pass~%"))
(defemit :try (body clauses)
  (fresh)
  (out "try:~%")
  (indent
   (emit body))  
  (iter
    (fresh)
    (for clause in clauses)
    (ecase (first clause)
      (:except
       (if (null (second (second clause)))
	   (out "except ~A:~%" (first (second clause)))
	   (out "except ~A as ~A:~%" (first (second clause))
		 (second (second clause))))
       (indent (emit (third clause))))
      ((:else :finally)
       (out "~A:~%" (case (first clause) (:else "else") (:finally "finally")))
       (indent (emit (second clause)))))))

(defemit :def (name args &rest body)
  (fresh)
  (out "def ~A (~A):~%" name (arg-list args))
  (indent (emit-block body)))

(defemit :while (test &rest body)
  (fresh)
  (out "while ~A:~%" (emit! test))
  (indent (emit-block body)))

(defemit :import (module)
  (assert (symbolp module))
  (fresh)
  (out "import ~A~%" module))

(defemit :from (package _ modules)
  (assert (every #'symbolp (mklst modules)))
  (assert (symbolp package))
  (assert (equalp _ 'import))
  (fresh)
  (out "from ~A import ~A~%" package (arg-list (mklst modules))))

(defemit :progn (&rest body)
  (emit-block body))

(defemit :class (name super &rest body)
  (fresh)
  (out "class ~A ~A:~%" name super)
  (indent (emit-block body)))

(defemit := (variable value)
  (fresh)
  (out "~A = ~A~%" variable (emit! value)))

(defemit :if (test then &optional else)
  (fresh)
  (out "if ~A:~%" (emit! test))
  (indent (emit then))
  (when else
    (fresh)
    (out "else:~%")
    (indent (emit else))))

(defemit :break nil
  (fresh)
  (out "break~%"))

(defemit :function (name &rest args)
  (out "~A(~A)" name (arg-list args)))

(defemit :atom val
  (if (stringp val)
      (out "~S" val)
      (out "~A" val)))

(define-multiple-binary + - * / % ** << >> > >= < <= and or)
(define-single-binary (= ==) in)
							  



