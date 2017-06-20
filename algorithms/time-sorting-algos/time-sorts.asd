(defsystem :time-sorts
  :depends-on (:cl-ana :alexandria :uiop :cl-ppcre :iterate :cl-csv)
  :serial t
  :components ((:file "package")
               (:file "time-sorts")))
