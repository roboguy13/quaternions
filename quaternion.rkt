#lang racket

(require "matrix.rkt")

(provide quaternion-sum
         quaternion-prod
         quaternion-conj
         quaternion-mag
         quaternion-inv
         quaternion-eq?
         quaternion-diff
         quaternion-div
         quaternion-log
         quaternion-exp
         quaternion-expt)

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

(define (quaternion-prod a b)
     ; Convert between row vector and column vector form
     ; For example (row-vector->column-vector '(1 2 3 4))  ->  '((1) (2) (3) (4))
  (define (row-vector->column-vector v)
    (map list v))
  (define (column-vector->row-vector v)
    (map car v))

  (column-vector->row-vector
   (matrix-prod (quaternion->matrix a)
                (row-vector->column-vector b))))


;--The conjugate of a quaternion
;--If q is a unit quaternion, the conjugate
;--is equal to the inverse.
(define (quaternion-conj q)
  (cons (car q) (map - (cdr q))))
  
  
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
