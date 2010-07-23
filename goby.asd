(defsystem :goby
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "macro-core")
   (:file "macros")
   (:file "unroll-utils")
   (:file "unroll"))
  :depends-on (iterate metabang-bind))
