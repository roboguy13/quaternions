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
