(cl:in-package :cl)

(defpackage :info.read-eval-print.html
 (:use :cl)
 (:export #:html
          #:*html-output*
          #:raw
          #:*html-pprint*))
