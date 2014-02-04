#lang web-server/insta

(require "eval.rkt")
(require mzlib/string)


;----------------------------------------------------;
; This is a direct copy-paste from my final project, ;
; with a few modifications. Work in progress.        ;
;----------------------------------------------------;

(struct blog (posts) #:mutable)
(struct post (expression))
(define BLOG
  (blog '()))

(define (start request) (render-blog-page request))

(define (parse-post bindings)
  (post (extract-binding/single 'expression bindings)))

(define (render-blog-page request)
  (local [(define (response-generator make-url)
            (response/xexpr
             `(html (head (title "Quaternion Evaluation"))
                    (body
                     (h1 "Quaternion Evaluation") 
                     (h2 "A project by Team Fourth Dimension")
                     (form((action
                            ,(make-url insert-post-handler)))
                          "Expression to Evaluate:"
                          (br)
                          (input ((name "expression") (value "(* 2i+k 4+j+8k)")))
                          (input ((type "submit"))))
                     (a ((href "http://mathworld.wolfram.com/Quaternion.html"))
                        "What is a quaternion?")
                     (div ((id "history")) ,(render-posts))
                     (br)))))
          (define (insert-post-handler request)
            (blog-insert-post!
             BLOG (parse-post (request-bindings request)))
            (render-blog-page request))]
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
            (p,"Output: ",(expr->string(quaternion-eval expr)))))


;--Put CSS here
(define (render-posts)
  `(div ((class "posts"))
        '(style ((type "text/css"))
                "body { text-align:center; background-image:url('http://wallpoper.com/images/00/22/77/94/pattern-other_00227794.jpg') }"
                "form { font-family: Verdana; color:#ddd; text-align:center }"
                "input { font-family: Verdana; margin:3px }"
                "h1 { font-family: Helvetica; color:#ddd; padding-top:20px }"
                "h2 { font-family: Helvetica; color:#c00; margin-top:-20px; font-size:16px }"
                "h3 { font-family: Helvetica; color:#ddd; font-size:20px }"
                "#history { width:940px; overflow:hidden; margin: 10px auto; padding 0 0 20px 20px; background:#333; border-radius: 15px 15px 15px 15px}"
                "p { font-family: Verdana; color:#ddd }"
                "a:link {color:#aaa}"
                "a:visited {color:#777}")
        (h3 "History:")
        ,@(map render-post (blog-posts BLOG))))
