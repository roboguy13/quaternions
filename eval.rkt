#lang racket

(require "operations.rkt")
(require "matrix.rkt")

(provide quaternion-eval)

;--If the string is a number, returns it as a number, otherwise as a symbol
(define (string->symOrNum str)
  (if (and (number? (string->number str))
           (not (and (complex? (string->number str))
                     (not (real? (string->number str))))))
      (string->number str)
      (string->symbol str)))

;--These are used for readability
(define (last L)
  (string-length (~a L)))

(define (penultimate L)
  (- (string-length (~a L)) 1))

(define (coeff L) ;before i, j, or k (or any single character)
  (string->number (substring (~a L) 0 (penultimate L))))

(define (first L) (cadr L)) ;first after an operation
(define (second L) (caddr L)) ;second after an operation
(define (restOf L) (cddr L)) ;everything after an operation except first (rest is a keyword already)
(define (arguments E) (cdr E)) ;since car is the operation (+ - exp log ...)


(define quaternion-rx #rx"(\\+|\\-)?[0-9\\./i-k]+|[0-9]+")

;--This can handle L in both cases:
;--  '(+ ## ##i ...) - not necessarily complete
;--  '##+##i+... - not necessarily complete
;--Note that there are no spaces between numbers and + symbols in the latter example
(define (addition->quaternion L-orig)
  (define (go L)
    (cond
      ((null? L) '(0 0 0 0))
      ((number? L) (list L 0 0 0)) ;real number
      ((symbol? L) 
       (case L
         ((i) (list 0 1 0 0))
         ((j) (list 0 0 1 0))
         ((k) (list 0 0 0 1))
         (else
          (let ((chars (string->list (~a L))))
          (if (or (equal? (length chars) 1) ;a single i, j, or k along with its sign
                  (and (equal? (length chars) 2)
                       (or (eq? (car chars) #\+)
                           (eq? (car chars) #\-))))
              (go (string->symbol (list->string (cons (car chars) (cons #\1 (cdr chars)))))) ;puts a 1 as the coeffecient
              (case (substring (~a L) (penultimate L) (last L)) ;##i, ##j, or ##k
                (("i") (list 0 (coeff L) 0 0))
                (("j") (list 0 0 (coeff L) 0))
                (("k") (list 0 0 0 (coeff L)))))))))
      ((eq? '+ (car L)) ;(+ ## ##i ...) or (+) from recursion
       (if(null? (cdr  L)) ;just (+)
          '(0 0 0 0)
          (vector-sum (go (first L))
                      (go (cons '+ (restOf L))))))))
  (cond
    ((regexp-match? quaternion-rx (~a L-orig)) ;this splits #+#i... to (+ # #i ...) and sends it back through
     (go (cons '+ (map string->symOrNum (regexp-match* quaternion-rx (~a L-orig))))))
    (else
     "Unexpected Arguments")))

;--Checks to see if any of the arguments is a string
(define (stringexists? E . F)
  (cond
    ((null? E) #f)
    ((string? E) (print E) (newline) #t)
    ((list? E) (or (stringexists? (car E))
                   (stringexists? (cdr E))
                   (stringexists? F)))
    (else (stringexists? F))))

(define (error f E)
  (if (stringexists? E)
      "error: Unexpected Arguments."
      (f E)))

(define (error2 f E F)
  (if (stringexists? E F)
      "error2: Unexpected Arguments."
      (f E F)))

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
    ((and (symbol? E)
          (regexp-match? quaternion-rx (~a E)))
       (addition->quaternion E)) ;in the form of #+#i+...
    ((or (symbol? E) (string? E)) "Unexpected Arguments.") ;Symbol, but not complex number (caught above)
    (else
     (case (car E)
       ((+) (cond
              ((equal? (length E) 1) 0) ;no arguments - 0 per Scheme Standard
              ((equal? (length (arguments E)) 1) (error quaternion-eval (first E))) ;one argument - equals itself
              (else (error2 quaternion-sum
                        (quaternion-eval (first E))
                        (quaternion-eval (cons '+ (restOf E)))))))
       ((-) (if (equal? (length (arguments E)) 1) ;just one argument, so negative, not subtraction
                (error quaternion-eval (list '* -1 (first E)))
                (error2 quaternion-diff
                    (quaternion-eval (first E))
                    (map quaternion-eval (restOf E)))))
       ((*) (cond
              ((equal? (length E) 1) 1) ;no arguments - 1 per Scheme Standard
              ((equal? (length (arguments E)) 1) (error quaternion-eval (first E))) ;one argument - equals itself
              (else (error2 quaternion-prod
                        (quaternion-eval (first E))
                        (quaternion-eval (cons '* (restOf E)))))))
       ((/) (if (equal? (length (arguments E)) 1) ;one argument - inverse, not division
                (error quaternion-eval (list '/ 1 (first E)))
                (error2 quaternion-div
                    (quaternion-eval (first E))
                    (map quaternion-eval (restOf E)))))
       ((exp) (error quaternion-exp (quaternion-eval (first E))))
       ((expt) (error2 quaternion-expt
                   (quaternion-eval (first E))
                   (quaternion-eval (second E))))
       ((log) (error quaternion-log (quaternion-eval (first E))))
       ((sin) (error quaternion-sin (quaternion-eval (first E))))
       ((cos) (error quaternion-cos (quaternion-eval (first E))))
       ((magnitude) (error quaternion-mag (quaternion-eval (first E))))
       
       ((=) (error2 quaternion-eq?
                   (quaternion-eval (first E))
                   (map quaternion-eval (restOf E))))
       (else E)
     ))))
