#lang racket

(provide vector-sum
         matrix-sum
         transpose
         matrix-prod
         matrix-scalar-prod)

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
