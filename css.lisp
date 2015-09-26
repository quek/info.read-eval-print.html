(in-package :info.read-eval-print.html)

(defun css-to-s (x)
  (typecase x
    (symbol
     (let ((name (symbol-name x)))
       (if (some #'lower-case-p name)
           name
           (string-downcase name))))
    (t (princ-to-string x))))

(defun css-split-properties (form &optional acc)
  (let ((pos (position-if #'keywordp form :start 1)))
    (if pos
        (css-split-properties (subseq form pos) (cons (subseq form 0 pos) acc))
        (nreverse (cons form acc)))))

(defun write-css-selector (selector)
  (format *html-output* "~&~{~a~^ ~}" (mapcar #'css-to-s selector)))

(defun write-css-property (property)
  (format *html-output* "~a:" (css-to-s (car property)))
  (format *html-output* "~{~a~^ ~};" (mapcar #'css-to-s (cdr property))))

(defun css-flatten (form &optional parent-selector)
  (if (null form)
      nil
      (let* ((pos (position-if (lambda (x) (or (keywordp x) (consp x))) form :start 1))
             (selector (subseq form 0 pos))
             (rest (subseq form pos))
             (properties (remove-if #'consp rest))
             (children (loop for child in (remove-if-not  #'consp rest)
                             nconc (css-flatten child (append parent-selector selector)))))
        (if properties
            (cons `(,(append parent-selector selector) ,properties)
                  children)
            children))))

(defun css (forms)
  (mapc (lambda (form)
          (loop for (selector properties) in (css-flatten form) do
            (write-css-selector selector)
            (write-char #\{ *html-output*)
            (mapc #'write-css-property (css-split-properties properties))
            (write-char #\} *html-output*)))
        forms))
