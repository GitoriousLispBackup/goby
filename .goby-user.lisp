
(defpackage :goby-user (:use :cl :goby))

(modern:copy-package :iterate :goby-user)
(in-package :goby-user)
(named-readtables:in-readtable :modern)
;;for right now, lets not do this, as it wont automatically update
;(modern:copy-package :goby :goby-user)

