(defsystem :bonus-project
  :depends-on (:iterate)
  :serial t
  :components ((:file "package")
               (:file "subjects")
               (:file "solutions")))
