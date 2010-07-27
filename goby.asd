(defsystem :goby
  :serial t
  :components
  (
   (:file "package")
   (:file "utils")
   (:file "macro-core")
   (:file "macros")
   (:file "unroll-utils")
   (:file "unroll")
   (:file "py-utils")
   (:file "py")
   
   (:file "user")
 
)
  :depends-on (iterate metabang-bind fset cl-irregsexp named-readtables
		       ;;for my repl for right now
		       iolib
		       ))
