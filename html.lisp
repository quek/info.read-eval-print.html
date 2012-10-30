(in-package :info.read-eval-print.html)

(defvar *html-output* *standard-output*)

(defmacro html (&body body)
  `(progn
     ,@(loop for i in body
             collect (walk-body i))
     (values)))

(defun escape (string)
  (with-output-to-string (out)
    (loop for c across string
          do (cond ((char= #\& c)
                    (write-string "&amp;" out))
                   ((char= #\< c)
                    (write-string "&lt;" out))
                   ((char= #\> c)
                    (write-string "&gt;" out))
                   ((char= #\" c)
                    (write-string "&quot;" out))
                   ((char= #\' c)
                    (write-string "&#x27;" out))
                   (t (write-char c out))))))

(defun walk-body (body)
  (cond ((null body)
         nil)
        ((and (consp body) (keywordp (car body)))
         (walk-tag body))
        (t
         `(emit ,body))))

(defun emit-raw-string (string)
  (write-string string *html-output*))

(defgeneric emit (thing))

(defmethod emit (thing)
  (emit (princ-to-string thing)))

(defmethod emit ((thing string))
  (write-string (escape thing) *html-output*))

(defmethod emit ((thing null))
  )

(defmethod emit ((thing raw))
  (write-string (raw-value thing) *html-output*))

(defstruct (raw (:constructor raw (value)))
  value)

(defun walk-tag (body)
  (multiple-value-bind (tag id classes) (parse-tag (car body))
    (multiple-value-bind (attributes body /-p) (parse-tag-args (cdr body))
      `(progn
         (emit-raw-string ,(with-output-to-string (out)
                             (format out "<~a" tag)
                             (when id
                               (format out " id=\"~a\"" id))
                             (when classes
                               (format out " class=\"~{~a~^ ~}\"" classes))))
         ,@(loop for (k . v) in attributes
                 collect `(emit-raw-string (format nil " ~a=\"~a\"" ,k (escape ,v))))
         ,(if /-p
              `(emit-raw-string "/>")
              `(progn
                 (emit-raw-string ">")
                 ,@(when body
                     `((html ,@body)
                       (emit-raw-string ,(format nil "</~a>" tag))))))))))

(defun parse-tag (tag)
  (let* ((str (string-downcase (symbol-name tag)))
         (p# (position #\# str))
         (p. (position #\. str)))
    (cond ((and (not p#) (not p.))
           str)
          ((and p# (not p.))
           (values (subseq str 0 p#)
                   (subseq str (1+ p#))))
          ((and (not p#) p.)
           (values (subseq str 0 p.)
                   nil
                   #1=(loop for start = (1+ p.) then (1+ end)
                            for end = (position #\. str :start start)
                            collect (subseq str start end)
                            unless end
                              do (loop-finish))))
          (t
           (values (subseq str 0 p#)
                   (subseq str (1+ p#) p.)
                   #1#)))))

(defun parse-tag-args (args)
  (let (attributes)
   (labels ((f (args)
              (if (and (consp args)
                       (keywordp (car args)))
                  (progn
                    (push (cons (string-downcase (car args)) (cadr args)) attributes)
                    (f (cddr args)))
                  (values (nreverse attributes)
                          args
                          (if (atom args)
                              args
                              (cdr (last args)))))))
     (f args))))
