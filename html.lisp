(in-package :info.read-eval-print.html)

(defvar *buffer* nil)

(defparameter *external-format* :utf-8)

(defmacro with-html-buffer ((&optional buffer) &body body)
  `(let ((*buffer* (or ,buffer (make-array 256 :adjustable t :fill-pointer 0))))
     ,@body
     *buffer*))

(defmacro html (&body body &environment env)
  `(progn
     ,@(concatenate-form (%html body env))
     nil))

(defun concatenate-form (form)
  (flet ((target-form-p (x)
           (and (consp x) (eq 'vector-push-extend (car x))))
         (concat-form (a b)
           (if a
               `(vector-push-extend
                 ,(concatenate '(simple-array (unsigned-byte 8) (*))
                               (cadr a) (cadr b))
                 *buffer*)
               b)))
    (let (r x)
      (loop for i in form
            do (cond ((target-form-p i)
                      (setf x (concat-form x i)))
                     (t
                      (when x (push x r) (setf x nil))
                      (push i r))))
      (when x (push x r))
      (nreverse r))))

(defun %html (body env)
  (loop for i in body
        for form = (walk-body i env)
        when form
          nconc form))

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

(defun start-tag-p (form)
  (and (consp form) (keywordp (car form))))

(defun walk-body (body env)
  (cond ((null body)
         nil)
        ((start-tag-p body)
         `(,@(walk-tag body env)))
        (t
         `(,@(*emit body env)))))

(defun emit-raw-string (string)
  (vector-push-extend (string-to-octets string) *buffer*))

(defun *emit-raw-string (string env)
  (if (constantp string env)
      `(vector-push-extend ,(string-to-octets string) *buffer*)
      `(emit-raw-string ,string)))

(defun *emit (thing env)
  (if (constantp thing env)
      (when thing
        `((vector-push-extend
           ,(string-to-octets (escape (princ-to-string thing)))
           *buffer*)))
      `((emit ,thing))))

(defgeneric emit (thing))

(defmethod emit (thing)
  (emit (princ-to-string thing)))

(defmethod emit ((thing string))
  (emit-raw-string (escape thing)))

(defmethod emit ((thing null)))

(defmethod emit ((thing raw))
  (emit-raw-string (raw-value thing)))

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

(defun walk-tag (body env)
  (multiple-value-bind (tag id classes) (parse-tag (car body))
    (multiple-value-bind (attributes body /-p) (parse-tag-args (cdr body))
      (when classes
        (let ((kv (assoc "class" attributes :test #'string=)))
          (when kv
            (setf (cdr kv) `(append ',classes
                                    (ensure-list ,(cdr kv)))
                  classes nil))))
      `(,(*emit-raw-string (with-output-to-string (out)
                             (format out "<~a" tag)
                             (when id
                               (format out " id=\"~a\"" (escape id)))
                             (when classes
                               (format out " class=\"~{~a~^ ~}\"" (mapcar #'escape classes))))
                           env)
        ,@(loop for (k . v) in attributes
                collect (let ((value (gensym)))
                          (if (constantp v env)
                              (if (eq v t)
                                  (*emit-raw-string (format nil " ~a" (escape k)) env)
                                  (*emit-raw-string (format nil " ~a=\"~a\"" (escape k)
                                                            (escape  v))
                                                    env))
                              `(let ((,value ,v))
                                 (cond ((eq ,value t)
                                        (emit-raw-string ,(format nil " ~a" (escape k))))
                                       (,value
                                        (emit-raw-string (format nil ,(format nil " ~a=\"~~a\"" (escape k))
                                                                 (escape ,value)))))))))
        ,@(cond ((and (string= "script" tag) (not body))
                 `(,(*emit-raw-string "></script>" env)))
                (/-p
                 `(,(*emit-raw-string " />" env)))
                (t `(,(*emit-raw-string ">" env)
                     ,@(when body
                         `(,@(%html body env)
                           ,(*emit-raw-string (format nil "</~a>" tag) env))))))))))

(defun ensure-list (x)
  (typecase x
    (list x)
    (t (list x))))

(defun string-to-octets (string)
  (sb-ext:string-to-octets string :external-format *external-format*))


#+nil
(defun f ()
  (loop for i across (print (html (:h1 "あ")
                              (:ul#a.d :class "c d" :data-foo "bar"
                               (:li.b "か")
                               (:li "さ"))))
          initially (terpri)
        do (write-string (sb-ext:octets-to-string i))))


#|
(loop for i across (print (html (:h1 "あ")
                            (:ul#a
                             (:li.b "か")
                             (:li "さ"))))
      do (write-string (sb-ext:octets-to-string i)))

(html (:h1 "あ")
  (:ul#a.d :class "c d" :data-foo "bar"
           (:li.b "か")
           (:li "さ")))

(html (:ul (loop for i from 1 to 3
                 do (html (:li i)))))

(html (:ul (html (:li 1))
        (html (:li 2))
        (html (:li 3))))

nil
|#
