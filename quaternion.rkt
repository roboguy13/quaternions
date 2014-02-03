#lang web-server/insta

(require mzlib/string)
(define-namespace-anchor a)
(define env (namespace-anchor->namespace a))

(define vector-sum
  (curry map +))

(define matrix-sum
  (curry map vector-sum))

(define (transpose m)
  (if (or (null? m)
          (null? (car m)))
      '()
      (cons (map car m)
            (transpose (map cdr m)))))


(define (matrix-prod orig-a orig-b)
  (define (go a b)
    (if (or (null? a)
            (null? b))
        '()
        (cons (map (curry foldr + 0)
                   (map (curry map * (car a))
                        b))
              (go (cdr a)
                  b))))
  (go orig-a (transpose orig-b)))

(define (matrix-scalar-prod a m)
  (map (curry map (curry * a))
       m))


(define (quaternion->matrix q)
  (matrix-sum
   (matrix-scalar-prod
    (car q)
    '(( 1  0  0  0)
      ( 0  1  0  0)
      ( 0  0  1  0)
      ( 0  0  0  1)))
   (matrix-scalar-prod
    (cadr q)
    '(( 0  1  0  0)
      (-1  0  0  0)
      ( 0  0  0 -1)
      ( 0  0  1  0)))
   (matrix-scalar-prod
    (caddr q)
    '(( 0  0  1  0)
      ( 0  0  0  1)
      (-1  0  0  0)
      ( 0 -1  0  0)))
   (matrix-scalar-prod
    (cadddr q)
    '(( 0  0  0  1)
      ( 0  0 -1  0)
      ( 0  1  0  0)
      (-1  0  0  0)))))

; Precondition: m must be a well-formed matrix representation of a quaternion.
(define (matrix->quaternion m)
  (car m))

; This function takes an operation that works on matrices uses
; it on quaternions.
; Precondition:
;   * qs must be a list of quaternions
;   * The given operation must respect the conversion between
;     matrices and quaternions, i.e. it must obey the law:
;
;       q is a valid quaterion => (f (quaternion->matrix q1) (quaternion->matrix q2) ...) is a well-formed matrix representation of a quaternion
(define (quaternion-op f . qs)
  (matrix->quaternion
       (apply f (map quaternion->matrix
                     qs))))

(define quaternion-sum
  (curry quaternion-op matrix-sum))

(define quaternion-prod
  (curry quaternion-op matrix-prod))
  
  
  
;--------------------------------------------------;
; These changes were added by Alexa.               ;
; But you can probably see that somewhere already. ;
;--------------------------------------------------;

;--Returns the magnitude of a quaternion
;--Note that this takes a quaternion in vector (list) form
(define (quaternion-mag Q)
  (sqrt (+ (expt (car Q) 2) (expt (cadr Q) 2) (expt (caddr Q) 2) (expt (cadddr Q) 2))))
  
;--Returns the norm of a vector
(define (norm V)
  (if (null? V)
      0
      (+ (expt (car V) 2) (norm (cdr V)))))

;--Returns the inverse of a quaternion
;--Takes a quaterion in vector form
(define (quaternion-inv Q)
  (if (list? (car Q))
      (quaternion-inv (car Q))
      (cons (/ (car Q) (norm Q)) (map (λ(x)(/ x (* (norm Q) -1))) (cdr Q)))))

;--Checks to see if two quaternions are equal
;--Somewhat unnecessary and can be taken out,
;--but I just put it in to keep quaternion operation syntax consistent
(define (quaternion-eq? Q R)
  (equal? Q R))

;--Multiplies a vector by constant C
(define (multiplyByC C V)
  (cond
    ((null? V) '())
    ((list? (car V)) (multiplyByC C (car V)))
    (else (cons (* (car V) C) (multiplyByC C (cdr V))))))
      
;--Subtracts Qs from Q, consistent with Scheme standard
;--Takes a quaternion in vector form
(define (quaternion-diff Q . Qs)
  (cond
    ((null? (car Qs)) Q)
    ((and (list? (caar Qs)) (equal? (length Qs) 1)) (quaternion-diff (quaternion-sum Q (multiplyByC -1 (caar Qs))) (cdar Qs)))
    (else (quaternion-diff (quaternion-sum Q (multiplyByC -1 (car Qs))) (cdr Qs)))))

;--Divides first Q by all other Qs, consistent with Scheme standard
;--Takes a quaternion in vector form
(define (quaternion-div Q . Qs)
  (cond
    ((null? (car Qs)) Q)
    ((and (list? (caar Qs)) (equal? (length Qs) 1)) (quaternion-div (quaternion-prod Q (quaternion-inv (caar Qs))) (cdar Qs)))
    (else (quaternion-div (quaternion-prod Q (quaternion-inv (car Qs))) (cdr Qs)))))
  
;---------------------------------------------;
; The following definitions were found at     ;
; http://www.lce.hut.fi/~ssarkka/pub/quat.pdf ;
;---------------------------------------------;

;--Gives the logarithm of a quaternion
;--Takes a quaternion in vector form
;--Note that log is our "ln" - the natural log
(define (quaternion-log Q)
  (if (equal? (cdr Q) '(0 0 0))
      (cons (log (car Q)) (cdr Q))
      (cons (log (sqrt (norm Q))) (multiplyByC (* (/ (sqrt (norm (cdr Q)))) (acos (/ (car Q) (sqrt (norm Q))))) (cdr Q)))))

;--Gives the exponential of a quaternion
;--Takes a quaternion in vector form
;--Note that this is e^quaternion
(define (quaternion-exp Q)
  (if (equal? (cdr Q) '(0 0 0))
      (cons (exp (car Q)) (cdr Q))
      (multiplyByC (exp (car Q)) (cons (cos (sqrt (norm (cdr Q)))) (multiplyByC (* (/ (norm (cdr Q))) (sin (norm (cdr Q)))) (cdr Q))))))

;--Gives Q^P, where at least one is a quaternion
;--Can take q^#, #^q, or q^q
;--Takes quaternions in vector form
(define (quaternion-expt Q P)
  (cond
    ((number? P) (quaternion-exp (multiplyByC P (quaternion-log Q))))
    ((number? Q) (quaternion-expt (cons Q '(0 0 0)) P))
    (else (quaternion-exp (quaternion-prod (quaternion-log Q) P)))))

;--- STILL NEED ---;
; quaternion-sin   ;
; quaternion-cos   ;
;------------------;
  
;--If the string is a number, returns it as a number, otherwise as a symbol
(define (string->symOrNum str)
  (if (number? (string->number str))
               (string->number str)
               (string->symbol str)))

;--This can handle L in both cases:
;--  '(+ ## ##i ...) - not necessarily complete
;--  '##+##i+... - not necessarily complete
;--Note that there are no spaces between numbers and + symbols in the latter example
(define (addition->quaternion L)
  (cond
    ((null? L) '(0 0 0 0))
    ((regexp-match? #rx"[1-9i-k]\\+" (~a L)) (addition->quaternion (cons '+ (map string->symOrNum (regexp-split #rx"\\+" (~a L)))))) ;first example and needs to be split
    ((number? L) (list L 0 0 0)) ;##
    ((symbol? L) (if (equal? (string-length (~a L)) 1) ;a single i, j, or k
                     (addition->quaternion (string->symbol (string-append "1" (~a L))))
                     (case (substring (~a L) (- (string-length (~a L)) 1) (string-length (~a L))) ;##i, ##j, or ##k
                       (("i") (list 0 (string->number (substring (~a L) 0 (- (string-length (~a L)) 1))) 0 0))
                       (("j") (list 0 0 (string->number (substring (~a L) 0 (- (string-length (~a L)) 1))) 0))
                       (("k") (list 0 0 0 (string->number (substring (~a L) 0 (- (string-length (~a L)) 1))))))))
    ((eq? '+ (car L)) ;(+ ## ##i ...) or (+) from recursion
     (if(null? (cdr  L)) ;just (+)
        '(0 0 0 0)
        (vector-sum (addition->quaternion (cadr L)) (addition->quaternion (cons '+ (cddr L)))))))) 


;---------------------------------------------;
; Here is the evaluation part of the program. ;
; It's still a work in progress, clearly.     ;
;---------------------------------------------;

;--Takes input in Scheme Standard notation
;--As it is, this returns a quaternion
(define (quaternion-eval E)
  (cond
    ((null? E) '())
    ((number? E) (addition->quaternion E))
    ((symbol? E) (addition->quaternion E)) ;in the form of #+#i+...
    (else
     (case (car E)
       ((+) (cond
              ((equal? (length E) 1) 0) ;no arguments - 0 per Scheme Standard
              ((equal? (length (cdr E)) 1) (quaternion-eval (cadr E))) ;one argument - equals itself
              (else (quaternion-sum (quaternion-eval (cadr E)) (quaternion-eval (cons '+ (cddr E))) ))))
       ((-) (if (equal? (length (cdr E)) 1) ;just one argument, so negative, not subtraction
                (quaternion-eval (list '* -1 (cadr E)))
                (quaternion-diff (quaternion-eval (cadr E)) (map quaternion-eval (cddr E)))))
       ((*) (cond
              ((equal? (length E) 1) 1) ;no arguments - 1 per Scheme Standard
              ((equal? (length (cdr E)) 1) (quaternion-eval (cadr E))) ;one argument - equals itself
              (else (quaternion-prod (quaternion-eval (cadr E)) (quaternion-eval (cons '* (cddr E))) ))))
       ((/) (if (equal? (length (cdr E)) 1) ;one argument - inverse, not division
                (quaternion-eval (list '/ 1 (cadr E)))
                (quaternion-div (quaternion-eval (cadr E)) (map quaternion-eval (cddr E)))))
       ((exp) (quaternion-exp (quaternion-eval (cadr E))))
       ((expt) (quaternion-expt (quaternion-eval (cadr E)) (quaternion-eval (caddr E))))
       ((log) (quaternion-log (quaternion-eval (cadr E))))
       ((sin) '() )
       ((cos) '() )
       ((magnitude) (quaternion-mag (quaternion-eval (cadr E))))
     ))))





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
                     (form((action
                            ,(make-url insert-post-handler)))
                          "Quaternion to Evaluate:"
                          (br)
                          (input ((name "expression") (value "(* 2i+k 4+j+8k)")))
                          (input ((type "submit"))))
                     (a ((href "http://mathworld.wolfram.com/Quaternion.html"))
                        "What is a quaternion?")
                     ,(render-posts)
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
                "body { background-image:url('http://wallpoper.com/images/00/22/77/94/pattern-other_00227794.jpg') }"
                "form { font-family: Verdana; color:#ddd }"
                "input { font-family: Verdana; margin:3px }"
                "h1 { font-family: Helvetica; text-align:center; color:#ddd; margin:20px }"
                "p { font-family: Verdana; color:#ddd }"
                "a:link {color:#aaa}"
                "a:visited {color:#777}")
        ,@(map render-post (blog-posts BLOG))))
