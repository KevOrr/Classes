(defsystem :algo-class
  :depends-on (:cl-ana :alexandria :uiop :cl-ppcre :iterate :cl-csv)
  :components
  ((:file "package")
   (:file "time-sorting-algos/time-sorts" :depends-on ("package"))))
