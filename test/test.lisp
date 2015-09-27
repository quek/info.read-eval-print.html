(in-package :info.read-eval-print.html.test)

(def-suite html)
(in-suite html)


(defmacro html* (&body body)
  `(with-output-to-string (*html-output*)
     (html ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test html-pprint-nil
  (sb-cltl2:compiler-let ((*html-pprint* nil))

    (is (string= "hello"
                 (html* "hello")))

    (is (string= "hello"
                 (html* "hello")))


    (is (string= "&<>\"'"
                 (html* (raw "&<>\"'"))))

    (is (string= "<p>hello</p>"
                 (html* (:p "hello"))))

    (is (string= "<br />"
                 (html* (:br . /))))

    (is (string= "<meta charset=\"utf-8\">"
                 (html* (:meta :charset "utf-8"))))

    (is (string= "<p id=\"foo\">あ</p>"
                 (html* (:p#foo "あ"))))

    (is (string= "<p class=\"foo\">あ</p>"
                 (html* (:p.foo "あ"))))

    (is (string= "<p class=\"foo bar\">あ</p>"
                 (html* (:p.foo.bar "あ"))))

    (is (string= "<p id=\"baha\" class=\"foo bar\">あ</p>"
                 (html* (:p#baha.foo.bar "あ"))))

    (is (string= "<p id=\"baha\" class=\"foo bar\" data-foo=\"true\">あ</p>"
                 (html* (:p#baha.foo.bar :data-foo "true" "あ"))))

    (is (string= "<p id=\"baha\" class=\"foo bar\" data-foo=\"true\" style=\"display: none;\">あ</p>"
                 (html* (:p#baha.foo.bar :data-foo "true" :style "display: none;" "あ"))))

    (is (string= "<div>a<div>&lt;b&gt;</div></div>"
                 (html* (:div "a" (:div "<b>")))))

    (is (string= "<a href=\"#\" onclick=\"alert(&#x27;&lt;hello&gt;&#x27;);\">あ</a>"
                 (html* (:a :href "#" :onclick "alert('<hello>');" "あ"))))

    (is (string= "<div>&amp;hi</div>"
                 (html* (:div (concatenate 'string "&" "hi")))))

    (is (string= "<ul><li>1</li><li>2</li><li>3</li></ul>"
                 (html* (:ul (loop for i from 1 to 3
                                   do (html (:li i)))))))

    (is (string= "<ul><li>1</li><li>2</li><li>3</li></ul>"
                 (html* (:ul (html (:li 1))
                             (html (:li 2))
                             (html (:li 3))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test html-pprint-t
  (sb-cltl2:compiler-let ((*html-pprint* t))

    (is (string= "<!DOCTYPE html>
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

    (is (string= "<ul>
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

    (is (string= "<div>
  <div>
    <textarea>foo
bar</textarea>
  </div>
</div>
" (html* (:div (:div (:textarea (format nil "foo~%bar")))))))

    (is (string= "<script src=\"a.js\"></script>
" (html* (:script :src "a.js"))))

    (is (string= "<div class=\"a b\">
  c
</div>
" (html* (:div :class '("a" "b") "c"))))

    (is (string= "<div class=\"z a0 a1 a2\">
  c
</div>
" (html* (:div.z :class (loop for i to 2 collect (format nil "a~a" i)) "c"))))

    (is (string= "<div a=\"b\">
  c
</div>
" (html* (:div :a "b" "c"))))

    (is (string= "<div a>
  c
</div>
" (html* (:div :a t "c"))))

    (is (string= "<div>
  c
</div>
" (html* (:div :a nil "c"))))

    (is (string= "<div a>
  c
</div>
" (html* (:div :a (= 1 1) "c"))))

    (is (string= "<div>
  c
</div>
" (html* (:div :a (= 1 2) "c"))))
    ))

(debug!)
