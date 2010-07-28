;;code that produces errors
(print (import matplotlib.pyplot as plt))

(class test ())
(def hi () (print 2))
c
(let ((a (dict))) (print (a.has_key 2)))
(let ((a (dict))) (print (.has_key a 2)))
(let ((a (dict))) (print (. a (has_key 2))))