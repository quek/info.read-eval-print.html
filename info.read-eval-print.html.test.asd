(asdf:defsystem :info.read-eval-print.html.test
  :serial t
  :pathname "test/"
  :components ((:file "package")
               (:file "test"))
  :depends-on (:info.read-eval-print.html
               :fiveam))
