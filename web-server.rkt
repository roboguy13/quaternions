#lang web-server/insta

(require "eval.rkt")
(require "quaternion.rkt")  ; For show-quaternion
(require mzlib/string)

(struct blog (posts) #:mutable)
(struct post (expression))
(define BLOG
  (blog '()))

(define (start request) (render-page request))

(define (parse-post bindings)
  (post (extract-binding/single 'expression bindings)))

(define (render-page request)
  (local [(define (response-generator make-url)
            (response/xexpr
             `(html 
               (div ((id "top"))
                    (div ((id "inner")) "Team Fourth Dimension"))
               (div ((id "menubar"))
                    (div ((id "title")) (h1 "Quaternion Evaluation")))
               (div ((id "main"))(head (title "Quaternion Evaluation"))
                    (body
                     (form((action
                            ,(make-url insert-post-handler)))
                          "Expression to Evaluate:"
                          (br)
                          (input ((name "expression") (size "30")))
                          (input ((type "submit"))))
                     (div ((id "history")) ,(render-posts))
                     (br)))
               (div ((id "menubar"))
                    (div ((id "info")) (h2 (a ((href "http://mathworld.wolfram.com/Quaternion.html") (target "_blank")) "What are Quaternions?")))))
             ))
          (define (insert-post-handler request)
            (blog-insert-post!
             BLOG (parse-post (request-bindings request)))
            (render-page request))]
    (send/suspend/dispatch response-generator)))

(define (blog-insert-post! a-blog a-post)
  (set-blog-posts! a-blog (cons a-post (blog-posts a-blog))))


(define (render-post a-post)
  (define expr (if (equal? (string-length (post-expression a-post)) 0)
                   '()
                   (read-from-string(post-expression a-post))))
  `(div ((class "post"))
        (hr)
        (p,"Input: ", (expr->string expr))
        (p,"Output: ",(show-quaternion (quaternion-eval expr)))))


;--Put CSS here
(define (render-posts)
  `(div ((class "posts"))
        '(style ((type "text/css"))
                "body { text-align:center; background-image:url('http://wallpoper.com/images/00/22/77/94/pattern-other_00227794.jpg') }"
                "form { font-family: Verdana; color:#222; text-align:center;margin:20px 0 20px 0 }"
                "input { font-family: monospace; margin:8px 5px 0 0 }"
                "h1 { font-family: Helvetica; color:#ddd; margin:5px 0 5px 0; font-size:36px }"
                "h2 { font-family: Helvetica; color:#999; margin-top:-20px; font-size:16px }"
                "h3 { font-family: Helvetica; color:#ddd; font-size:20px }"
                "#history { width:800px; overflow:hidden; margin: 10px auto 15px; padding 0 0 20px 20px; background:#222; border-radius: 15px 15px 15px 15px}"
                "#main { width:900px; overflow:hidden; margin: 10px auto; padding 0 0 20px 20px; background-image:url('http://i923.photobucket.com/albums/ad74/rblymire79/BrushedMetal.jpg'); border-radius: 15px 15px 15px 15px}"
                "#top { width: 900px; margin: -10px auto 0}"
                "#inner { width:200px; font-family:Verdana; font-variant:small-caps; color:#ddd; overflow:hidden; margin: 0 0 0 700px; padding:5px 0 5px 0 ; background:#b00; border-radius: 0 0 5px 5px}"
                "#menubar {width:900px; height:45px; margin: 0 auto 0; border-radius:15px 15px 0 0}"
                "#title {width: 500px; float:left; text-align:left; height:45px; margin:0 auto -5px; padding-left:10px; background:transparent}"
                "#info {width: 400px; float:right; text-align:right; padding-right:10px; padding-top:20px; background:transparent}"
                "p { font-family: Verdana; color:#ddd }"
                "a:link { font-family: Helvetica; color:#999; font-size:16px; padding-bottom:10px}"
                "a:visited {font-family: Helvetica; color:#999; font-size:16px; padding-bottom:10px}")
        (h3 "Evaluation History:")
        ,@(map render-post (blog-posts BLOG))))
