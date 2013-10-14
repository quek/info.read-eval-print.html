(in-package :cl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (quicklisp:quickload :info.read-eval-print.html))

(defpackage :info.read-eval-print.html.test
  (:use :cl :info.read-eval-print.html))

(in-package :info.read-eval-print.html.test)

(defmacro html* (&body body)
  `(with-output-to-string (*html-output*)
     (html ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *html-pprint* nil)

(assert (string= "hello"
                 (html* "hello")))

(assert (string= "&<>\"'"
                 (html* (raw "&<>\"'"))))

(assert (string= "<p>hello</p>"
                 (html* (:p "hello"))))

(assert (string= "<br />"
                 (html* (:br . /))))

(assert (string= "<meta charset=\"utf-8\">"
                 (html* (:meta :charset "utf-8"))))

(assert (string= "<p id=\"foo\">あ</p>"
                 (html* (:p#foo "あ"))))

(assert (string= "<p class=\"foo\">あ</p>"
                 (html* (:p.foo "あ"))))

(assert (string= "<p class=\"foo bar\">あ</p>"
                 (html* (:p.foo.bar "あ"))))

(assert (string= "<p id=\"baha\" class=\"foo bar\">あ</p>"
                 (html* (:p#baha.foo.bar "あ"))))

(assert (string= "<p id=\"baha\" class=\"foo bar\" data-foo=\"true\">あ</p>"
                 (html* (:p#baha.foo.bar :data-foo "true" "あ"))))

(assert (string= "<p id=\"baha\" class=\"foo bar\" data-foo=\"true\" style=\"display: none;\">あ</p>"
                 (html* (:p#baha.foo.bar :data-foo "true" :style "display: none;" "あ"))))

(assert (string= "<div>a<div>&lt;b&gt;</div></div>"
                 (html* (:div "a" (:div "<b>")))))

(assert (string= "<a href=\"#\" onclick=\"alert(&#x27;&lt;hello&gt;&#x27;);\">あ</a>"
                 (html* (:a :href "#" :onclick "alert('<hello>');" "あ"))))

(assert (string= "<div>&amp;hi</div>"
                 (html* (:div (concatenate 'string "&" "hi")))))

(assert (string= "<ul><li>1</li><li>2</li><li>3</li></ul>"
                 (html* (:ul (loop for i from 1 to 3
                                   do (html (:li i)))))))

(assert (string= "<ul><li>1</li><li>2</li><li>3</li></ul>"
                 (html* (:ul (html (:li 1))
                          (html (:li 2))
                          (html (:li 3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *html-pprint* t)

(assert (string= "<!DOCTYPE html>
<html>
  <head>
    <meta charset=\"utf-8\">
    <meta name=\"author\">
      Common Lisp
    </meta>
    <title>
      Hello
    </title>
  </head>
  <body>
    <section>
      <p>
        bye
        bye
      </p>
      <p>
        hello
      </p>
    </section>
  </body>
</html>
" (html* (raw "<!DOCTYPE html>")
    (:html
      (:head
          (:meta :charset "utf-8")
        (:meta :name "author" "Common Lisp")
        (:title "Hello"))
      (:body
          (:section
           (:p "bye" "bye")
           (:p "hello")))))))

(assert (string= "<ul>
  <li>
  1
</li>
<li>
  2
</li>
<li>
  3
</li>

</ul>
" (html* (:ul (loop for i from 1 to 3
                    do (html (:li i)))))))

(assert (string= "<div>
  <div>
    <textarea>foo
bar</textarea>
  </div>
</div>
" (html* (:div (:div (:textarea (format nil "foo~%bar")))))))

(assert (string= "<script src=\"a.js\"></script>
" (html* (:script :src "a.js"))))

(assert (string= "<div class=\"a b\">
  c
</div>
" (html* (:div :class '("a" "b") "c"))))

(assert (string= "<div class=\"z a0 a1 a2\">
  c
</div>
" (html* (:div.z :class (loop for i to 2 collect (format nil "a~a" i)) "c"))))

(assert (string= "<div a=\"b\">
  c
</div>
" (html* (:div :a "b" "c"))))

(assert (string= "<div a>
  c
</div>
" (html* (:div :a t "c"))))

(assert (string= "<div>
  c
</div>
" (html* (:div :a nil "c"))))

(assert (string= "<div a>
  c
</div>
" (html* (:div :a (= 1 1) "c"))))

(assert (string= "<div>
  c
</div>
" (html* (:div :a (= 1 2) "c"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro css* (&body body)
  `(with-output-to-string (*html-output*)
     (css ,@body)))

(assert
 (string= "body {
  color: #000000;
}
" (css* (body (color %000000)))))

(assert
 (string= "h1 {
  margin: 0 2px 0 4px;
  color: red;
}
h2 {
  text-align: center;
}
h3, h4 {
  color: red;
}
a:hover {
  font-size: 12px;
}
" (css*
       (h1
        (margin 0 2px 0 4px)
        (color red))
    (h2 (text-align center))
    ((or h3 h4)
     (color red))
    (a%hover
     (font-size 12px)))))
