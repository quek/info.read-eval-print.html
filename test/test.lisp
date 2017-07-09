(in-package :info.read-eval-print.html.test)

(defmacro s (&body body)
  `(with-output-to-string (out)
     (loop for i across (with-html-buffer ()
                          ,@body)
           do (write-string (sb-ext:octets-to-string i) out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(alexandria:define-constant +hello+ "Hello!" :test #'string=)

(deftest test-basic ()
  (is (string= "hello"
               (s (html "hello"))))

  (is (string= "Hello!"
               (s (html +hello+))))


  (is (string= "&<>\"'"
               (s (html (raw "&<>\"'")))))

  (is (string= "<p>hello</p>"
               (s (html (:p "hello")))))

  (is (string= "<br />"
               (s (html (:br . /)))))

  (is (string= "<meta charset=\"utf-8\">"
               (s (html (:meta :charset "utf-8")))))

  (is (string= "<p id=\"foo\">あ</p>"
               (s (html (:p#foo "あ")))))

  (is (string= "<p class=\"foo\">あ</p>"
               (s (html (:p.foo "あ")))))

  (is (string= "<p class=\"foo bar\">あ</p>"
               (s (html (:p.foo.bar "あ")))))

  (is (string= "<p id=\"baha\" class=\"foo bar\">あ</p>"
               (s (html (:p#baha.foo.bar "あ")))))

  (is (string= "<p id=\"baha\" class=\"foo bar\" data-foo=\"true\">あ</p>"
               (s (html (:p#baha.foo.bar :data-foo "true" "あ")))))

  (is (string= "<p id=\"baha\" class=\"foo bar\" data-foo=\"true\" style=\"display: none;\">あ</p>"
               (s (html (:p#baha.foo.bar :data-foo "true" :style "display: none;" "あ")))))

  (is (string= "<div>a<div>&lt;b&gt;</div></div>"
               (s (html (:div "a" (:div "<b>"))))))

  (is (string= "<a href=\"#\" onclick=\"alert(&#x27;&lt;hello&gt;&#x27;);\">あ</a>"
               (s (html (:a :href "#" :onclick "alert('<hello>');" "あ")))))

  (is (string= "<div>&amp;hi</div>"
               (s (html (:div (concatenate 'string "&" "hi"))))))

  (is (string= "<ul><li>1</li><li>2</li><li>3</li></ul>"
               (s (html (:ul (loop for i from 1 to 3
                                   do (html (:li i))))))))

  (is (string= "<ul><li>1</li><li>2</li><li>3</li></ul>"
               (s (html (:ul (html (:li 1))
                          (html (:li 2))
                          (html (:li 3))))))))

(deftest test-escape ()
  (is (string= "<p id=\"&lt;id&gt;\" class=\"&lt;class&gt;\" data-x=\"&lt;data&gt;\">&lt;body&gt;</p>"
               (s (html (:p#<id>.<class> :data-x "<data>" "<body>"))))))

(run-package-tests :interactive t)
