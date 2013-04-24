(in-package :info.read-eval-print.html)

(defvar *html-output* *standard-output*)

(defvar *html-pprint* t)

(defvar *indent* nil)

(defparameter *disable-pprint-tag* '("textarea"))

(defmacro html (&body body)
  `(let ((*indent* (or *indent* 0)))
     ,@(loop for i in body
             for form = (walk-body i)
             when form
             collect form)
     (values)))

(defstruct (raw (:constructor raw (value)))
  value)

(defgeneric escape (thing))

(defmethod escape ((thing string))
  (with-output-to-string (out)
    (loop for c across thing
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

(defmethod escape ((thing null))
  "")

(defmethod escape ((thing list))
  (format nil "~{~a~^ ~}" (mapcar #'escape thing)))

(defmethod escape (thing)
  (princ-to-string thing))

(defun walk-body (body)
  (cond ((null body)
         nil)
        ((and (consp body) (keywordp (car body)))
         `(progn
            (pre-block)
            ,(walk-tag body)
            (post-block)))
        (t
         (let ((value (gensym))
               (output (gensym)))
           `(if *html-pprint*
              (let (,value)
                (let ((,output (with-output-to-string (*html-output*)
                                 (setf ,value ,body))))
                  (if ,value
                      (progn
                        (pre-block)
                        (emit ,value)
                        (emit-raw-string ,output)
                        (post-block))
                      (emit-raw-string ,output))))
              (emit ,body))))))

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

(defun walk-tag (body)
  (multiple-value-bind (tag id classes) (parse-tag (car body))
    (multiple-value-bind (attributes body /-p) (parse-tag-args (cdr body))
      (when classes
        (let ((kv (assoc "class" attributes :test #'string=)))
          (when kv
            (setf (cdr kv) `(append ',classes
                                    (alexandria:ensure-list ,(cdr kv)))
                  classes nil))))
      `(progn
         (emit-raw-string ,(with-output-to-string (out)
                             (format out "<~a" tag)
                             (when id
                               (format out " id=\"~a\"" id))
                             (when classes
                               (format out " class=\"~{~a~^ ~}\"" classes))))
         ,@(loop for (k . v) in attributes
                 collect (let ((value (gensym)))
                           `(let ((,value ,v))
                              (cond ((eq ,value t)
                                     (emit-raw-string ,(format nil " ~a" k)))
                                    (,value
                                     (emit-raw-string (format nil ,(format nil " ~a=\"~~a\"" k)
                                                              (escape ,value))))))))
         ,(cond ((and (string= "script" tag) (not body))
                 `(emit-raw-string "></script>"))
                (/-p
                 `(emit-raw-string " />"))
                (t `(progn
                      (emit-raw-string ">")
                      ,@(when body
                          (let ((form `((post-start-tag)
                                        (html ,@body)
                                        (pre-end-tag)
                                        (emit-raw-string ,(format nil "</~a>" tag)))))
                            (if (member tag *disable-pprint-tag* :test #'string=)
                                `((let ((*html-pprint* nil))
                                    ,@form))
                                form))))))))))

(defun pre-block ()
  (when *html-pprint*
    (emit-indent)))

(defun post-block ()
  (when *html-pprint*
    (terpri *html-output*)))

(defun post-start-tag ()
  (when *html-pprint*
    (incf *indent*)
    (terpri *html-output*)))

(defun pre-end-tag ()
  (when *html-pprint*
    (decf *indent*)
    (emit-indent)))

(defun emit-indent ()
  (write-string (make-string (* 2 *indent*) :initial-element #\space)
                *html-output*))
