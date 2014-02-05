#lang racket/gui

; This is definitely a work in progress.

(require sgl/gl)
(require sgl/gl-vectors)

(require "quaternion.rkt")
(require "matrix.rkt")

(define (make-point x y z)
  (list x y z))

(define (make-square a b c d)
  (list a b c d))

(define (point->quaternion p)
  (cons 0 p))
(define (quaternion->point q)
  (cdr q))

; Precondition: axis must be a unit vector
(define (rotate-point axis angle p)
  (let* ((v     (vector-scalar-prod (sin (/ angle 2)) axis))
         (s     (cos (/ angle 2)))
         (q     (cons s v))
         (q-inv (quaternion-inv q))
         (pq    (point->quaternion p)))
    (quaternion->point
     (quaternion-prod q
                      (quaternion-prod pq
                                       q-inv)))))

; This rotation doesn't seem to work. The
; problem might not be in this function.
(define (rotate-square axis angle s)
  (map (curry rotate-point axis angle) s))

(define (resize w h)
  (glViewport 0 0 w h)
  #t)

(define *the-square*
  (make-square (make-point 0.25 0.25 0.0)
               (make-point 0.75 0.25 0.0)
               (make-point 0.75 0.75 0.0)
               (make-point 0.25 0.75 0.0)))

(define (render-square square)
  (glBegin GL_QUADS)
  (map (curry apply glVertex3d) square)
  (glEnd))

(define (draw-opengl)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)
  (glColor3d 1.0 1.0 1.0)
  
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho 0.0 1.0 1.0 0.0 0.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)

  (render-square *the-square*)

  (glFlush))


; This would be better if these were private
; fields in the gl-canvas% class but I don't
; remember the syntax for that.
(define go?     #t)
(define *update-time* 0)

(define gl-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers)
    
    (define/public (go) 
      (set! go? #t)
      (refresh))

    (define/override (on-paint)
        (with-gl-context
         (lambda ()
           (let ((curr-time (current-process-milliseconds)))
                   ; 50 milliseconds per frame (20 fps) for testing
                   ; Lowering this number makes it flicker faster,
                   ; but does not actually remove the flicker.
             (when (> (abs (- curr-time *update-time*)) 50)                     
               (set! *the-square* (rotate-square '(0.25 0.25 0) 1 *the-square*))
               (set! *update-time* curr-time)))
           (when go?
             (draw-opengl)
             (swap-gl-buffers))))
      (when go?
        (set! go? #f)
        (queue-callback (lambda x (send this go)) #f)))

    (define/override (on-size width height)
      (with-gl-context
       (lambda ()
         (resize width height))))

    (define/override (on-char ch)
      (exit))

    (super-instantiate () (style '(gl)))))

(define window (new frame% (label "Rotation example") (min-width 250) (min-height 250)))
(define gl (new gl-canvas% (parent window)))

(send window show #t)
