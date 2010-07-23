(in-package :goby)
;-------------------------------------------------
(defemit :def (name args &rest body)
  (out "def ~A ~A:~%" name (arg-list args))
  (emit-block body))

(defemit :while (test &rest body)
  (out "while ~A:~%" (emit! test))
  (emit-block body))

(defemit :import (module)
  (assert (symbolp module))
  (out "import ~A~%" module))

(defemit :from (package &rest modules)
  (assert (every #'symbolp modules))
  (assert (symbolp package))
  (out "from ~A import ~A~%" package (arg-list modules)))

(defemit :progn (&rest body)
  (emit-block body))

(defemit :class (name super &rest body)
  (out "class ~A ~A:~%" name super)
  (emit-block body))

(defemit := (variable value)
  (out "~A = ~A~%" variable (emit! value)))

(defemit :if (test then &optional else)
  (out "if ~A:~%" (emit! test))
  (emit then)
  (when else
    (fresh)
    (out "else:~%")
    (emit else)))

(defemit :function (name &rest args)
  (out "~A(~A)" name (arg-list args)))

(defemit :atom (val) (out "~A" val))

;(define-multiple-binary + - * / % ** << >> and or)
;(define-single-binary =)

#|
(defbinary = (form1 form2)
  (out "(~A==~A)" (emit! form1) (emit! form2)))
(defbinary + (&rest args)
  (out "(~{~A~^+~})" (mapcar #'emit! args)))
|#
							  



