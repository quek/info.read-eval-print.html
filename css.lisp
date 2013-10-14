(in-package :info.read-eval-print.html)

(defmacro css (&body body)
  `(progn ,@(mapcar #'walk-selector body)
          (values)))

(defun walk-selector (css)
  `(progn
     (emit-raw-string ,(css-selector (car css)))
     (emit-raw-string " {")
     (emit-newline)
     ,@(mapcar #'walk-property (cdr css))
     (emit-raw-string "}")
     (emit-newline)))


(defun walk-property (property)
  `(progn
     (emit-raw-string "  ")
     (emit-raw-string ,(css-property (car property)))
     (emit-raw-string ": ")
     (emit-raw-string ,(css-value (cdr property)))
     (emit-raw-string ";") 
     (emit-newline)))



(defgeneric css-selector (x))

(defmethod css-selector ((x string))
  x)

(defmethod css-selector ((x symbol))
  (let ((str (string-downcase x)))
    (substitute #\: #\% str)))

(defmethod css-selector ((x list))
  (if (string-equal "OR" (car x))
      (format nil "~{~a~^, ~}" (mapcar #'css-selector (cdr x)))
      (format nil "~{~a~^ ~}" (mapcar #'css-selector x))))

(defgeneric css-property (x))

(defmethod css-property ((x string))
  x)

(defmethod css-property ((x symbol))
  (string-downcase x))

(defgeneric css-value (x))

(defmethod css-value (x)
  (princ-to-string x))

(defmethod css-value ((x string))
  x)

(defmethod css-value ((x symbol))
  (let ((str (string-downcase x)))
    (if (char= #\% (char str 0))
        (format nil "#~a" (subseq str 1))
        str)))

(defmethod css-value ((x list))
  (format nil "~{~a~^ ~}" (mapcar #'css-value x)))
