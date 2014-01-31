#lang racket

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
    ((symbol? L) (case (substring (~a L) (- (string-length (~a L)) 1) (string-length (~a L))) ;##i, ##j, or ##k
                   (("i") (list 0 (string->number (substring (~a L) 0 (- (string-length (~a L)) 1))) 0 0))
                   (("j") (list 0 0 (string->number (substring (~a L) 0 (- (string-length (~a L)) 1))) 0))
                   (("k") (list 0 0 0 (string->number (substring (~a L) 0 (- (string-length (~a L)) 1)))))))
    ((eq? '+ (car L)) ;(+ ## ##i ...) or (+) from recursion
     (if(null? (cdr  L)) ;just (+)
        '(0 0 0 0)
        (vector-sum (addition->quaternion (cadr L)) (addition->quaternion (cons '+ (cddr L)))))))) 