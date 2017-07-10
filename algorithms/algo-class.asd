(defsystem :algo-class
  :depends-on (:cl-ana :alexandria :uiop :cl-ppcre :iterate :cl-csv)
  :components
  ((:file "package")

   (:module :time-sorts
    :pathname #p"time-sorting-algos/"
    :depends-on ("package")
    :components
    ((:file "time-sorts")))

   (:module :bonus-project
    :pathname #p"bonus-project/"
    :depends-on ("package")
    :serial t
    :components
    ((:file "subjects")
     (:file "solutions")))))
