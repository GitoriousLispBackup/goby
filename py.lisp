(in-package :goby)
;;TODO: for some odd, weird reason, py takes *forever*!!! fix eventually!
;-------------------------------------------------
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

(defemit :function (name &rest args)
  (out "~A(~A)" name (arg-list args)))

(defemit :atom val (out "~S" val))

(define-multiple-binary + - * / % ** << >> and or)
(define-single-binary (= ==))
							  



