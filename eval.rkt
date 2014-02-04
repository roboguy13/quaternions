#lang racket

(require "quaternion.rkt")
(require "matrix.rkt")

(provide quaternion-eval)

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
