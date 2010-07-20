(defsystem :goby
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "env")
   (:file "walker-core")
   (:file "macro-core")
   (:file "walker")
   (:file "macro")
   (:file "goby"))
  :depends-on (iterate metabang-bind))
