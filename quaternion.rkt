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
         quaternion-expt
         quaternion-sin
         quaternion-cos)

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

(define (quaternion-prod qa qb)
  (let ((a1 (car    qa))
        (b1 (cadr   qa))
        (c1 (caddr  qa))
        (d1 (cadddr qa))
        
        (a2 (car    qb))
        (b2 (cadr   qb))
        (c2 (caddr  qb))
        (d2 (cadddr qb)))
    (list (-    (* a1 a2)
                (* b1 b2)
                (* c1 c2)
                (* d1 d2))
          
          (+    (* a1 b2)
                (* b1 a2)
                (* c1 d2)
                (- (* d1 c2)))
          
          (+    (* a1 c2)
                (- (* b1 d2))
                (* c1 a2)
                (* d1 b2))
          
          (+    (* a1 d2)
                (* b1 c2)
                (- (* c1 b2))
                (* d1 a2)))))



;--The conjugate of a quaternion
;--If q is a unit quaternion, the conjugate
;--is equal to the inverse.
(define (quaternion-conj q)
  (cons (car q) (map - (cdr q))))


;--------------------------------------------------;
; These changes were added by Alexa.               ;
; But you can probably see that somewhere already. ;
;--------------------------------------------------;

;--These definitions make the code more readable
(define (real Q) (car Q))
(define (imaginary Q) (cdr Q))
(define (i-coeff Q) (cadr Q))
(define (j-coeff Q) (caddr Q))
(define (k-coeff Q) (cadddr Q))

;--Returns the norm of a general vector
(define (norm V)
  (if (null? V)
      0
      (+ (expt (car V) 2)
         (norm (cdr V)))))

;--Returns the magnitude of a quaternion
;--Note that this takes a quaternion in vector (list) form
(define (quaternion-mag Q)
  (sqrt (norm Q)))

;--Returns the inverse of a quaternion
;--Takes a quaterion in vector form
(define (quaternion-inv Q)
  (if (list? (car Q)) ;this handles a case where Q is '((...)) instead of '(...)
      (quaternion-inv (car Q))
      (cons (/ (real Q) (norm Q))
            (map [λ(x)(/ x (* (norm Q) -1))] (imaginary Q)))))

;--Checks to see if two quaternions are equal
;--Somewhat unnecessary and can be taken out,
;--but I just put it in to keep quaternion operation syntax consistent
(define (quaternion-eq? Q R)
  (equal? Q R))

;--Multiplies a general vector by constant C
(define (multiplyByC C V)
  (cond
    ((null? V) '())
    ((list? (car V)) (multiplyByC C (car V))) ;V is '((...)) instead of '(...)
    (else (cons (* C (car V))
                (multiplyByC C (cdr V))))))

;--Subtracts Qs from Q, consistent with Scheme standard
;--Takes a quaternion in vector form
;--The way this works is as follows (with real numbers for simplicity):
;--(- 5 2 1 1) -> (- 3 1 1) -> (- 2 1) -> 1
;--It subtracts the second from the first, and then the rest from that, recursively
;--To subtract, adds the negative
(define (quaternion-diff Q . Qs)
  (cond
    ((null? (car Qs)) Q) ;Only one argument
    ((and (list? (caar Qs))(equal? (length Qs) 1)) (quaternion-diff
                                                    (quaternion-sum Q (multiplyByC -1 (caar Qs)))
                                                    (cdar Qs)))
    (else (quaternion-diff
           (quaternion-sum Q (multiplyByC -1 (car Qs)))
           (cdr Qs)))))

;--Divides first Q by all other Qs, consistent with Scheme standard
;--Takes a quaternion in vector form
;--Works similarly to quaternion-diff
;--To divide, multiplies by inverse
(define (quaternion-div Q . Qs)
  (cond
    ((null? (car Qs)) Q)
    ((and (list? (caar Qs)) (equal? (length Qs) 1)) (quaternion-div
                                                     (quaternion-prod Q (quaternion-inv (caar Qs)))
                                                     (cdar Qs)))
    (else (quaternion-div
           (quaternion-prod Q (quaternion-inv (car Qs)))
           (cdr Qs)))))

;---------------------------------------------;
; The following definitions were found at     ;
; http://www.lce.hut.fi/~ssarkka/pub/quat.pdf ;
;---------------------------------------------;

;--Gives the logarithm of a quaternion
;--Takes a quaternion in vector form
;--Note that log is our "ln" - the natural log
(define (quaternion-log Q)
  (if (equal? (imaginary Q) '(0 0 0)) ;a real number, and can use normal log function
      (cons (log (real Q))
            '(0 0 0))
      (cons (log (sqrt (norm Q)))
            (multiplyByC (* (/ (quaternion-mag (imaginary Q)))
                            (acos (/ (real Q) (quaternion-mag Q))))
                         (imaginary Q)))))

;--Gives the exponential of a quaternion
;--Takes a quaternion in vector form
;--Note that this is e^quaternion
(define (quaternion-exp Q)
  (if (equal? (imaginary Q) '(0 0 0)) ;a real number, and can just use normal exp function
      (cons (exp (real Q))
            '(0 0 0))
      (multiplyByC (exp (real Q))
                   (cons (cos (quaternion-mag (imaginary Q)))
                         (multiplyByC (* (/ (norm (imaginary Q))) (sin (norm (imaginary Q))))
                                      (imaginary Q))))))

;--Gives Q^P, where at least one is a quaternion
;--Can take q^#, #^q, or q^q
;--Takes quaternions in vector form
(define (quaternion-expt Q P)
  (cond
    ((number? P) (quaternion-exp (multiplyByC P (quaternion-log Q))))
    ((number? Q) (quaternion-expt (cons Q '(0 0 0)) P))
    (else (quaternion-exp (quaternion-prod (quaternion-log Q) P)))))

;-----------------------------------------------;
; I'm not sure whether or not this is correct.  ;
; This is how you take the cos / sin of complex ;
; numbers, so hopefully it translates?          ;
; Verified true when only real and i are used   ;
; in the quaternions. '(# # 0 0) - with cos.    ;
; sin is under examination. There is a problem  ;
; with code elsewhere here.                     ;
;-----------------------------------------------;

(define (quaternion-cos Q)
  (multiplyByC 0.5 (quaternion-sum
                      (quaternion-exp (quaternion-prod '(0 1 0 0) Q))
                      (quaternion-exp (quaternion-prod '(0 -1 0 0) Q)))))


(define (quaternion-sin Q)
  (quaternion-div (quaternion-diff
                     (quaternion-exp (quaternion-prod '(0 1 0 0) Q))
                     (quaternion-exp (quaternion-prod '(0 -1 0 0) Q)))
                 '(0 2 0 0)))
