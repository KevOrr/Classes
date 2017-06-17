(defsystem :time-sorts
  :depends-on (:cl-ana :alexandria :uiop :cl-ppcre :iterate)
  :serial t
  :components ((:file "package")
               (:file "time-sorts")))
