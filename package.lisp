(defpackage :goby (:use :cl :iterate) (:shadow :cl :let))
(in-package :goby)
(defmacro let (&rest args) `(metabang-bind:bind ,@args))
