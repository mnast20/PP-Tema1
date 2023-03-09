#lang racket

;(define (get-nth-tuple n tuple transform1 transform2 transform3)
;  (foldl (lambda (transformation tuple) (case transformation
;          ((1) (transform1 tuple))
;          ((2) (transform2 tuple))
;          ((3) (transform3 tuple))
;          )) tuple (get-transformations n))
; )

(define get-nth-tuple
  (lambda (n)
    (lambda (tuple)
      (lambda (transform1)
        (lambda (transform2)
          (lambda (transform3)
              (foldl (lambda (transformation tuple)
                       (case transformation
                         ((1) (transform1 tuple))
                         ((2) (transform2 tuple))
                         ((3) (transform3 tuple))
                         )) tuple (get-transformations n))
 )))))
 )

; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.
(define (get-nth-ppt-from-matrix-transformations n)
  ; (get-nth-tuple n '(3 4 5) (lambda (tuple) (multiply T1 tuple)) (lambda (tuple) (multiply T2 tuple)) (lambda (tuple) (multiply T3 tuple)))
  (((((get-nth-tuple n) '(3 4 5)) (lambda (tuple) (multiply T1 tuple))) (lambda (tuple) (multiply T2 tuple))) (lambda (tuple) (multiply T3 tuple)))
  )


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.
(define (get-nth-quadruple n)
  ; (get-nth-tuple n '(1 1 2 3) (lambda (tuple) (apply Q1 tuple)) (lambda (tuple) (apply Q2 tuple)) (lambda (tuple) (apply Q3 tuple)))
  (((((get-nth-tuple n) '(1 1 2 3)) (lambda (tuple) (apply Q1 tuple))) (lambda (tuple) (apply Q2 tuple))) (lambda (tuple) (apply Q3 tuple)))
  )


; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.
(define (get-nth-ppt-from-GH-quadruples n)
  (define quadruple (get-nth-quadruple n))
  (define g (car quadruple))
  (define e (cadr quadruple))
  (define f (caddr quadruple))
  (define h (cadddr quadruple))
  
  (list (* g h) (* 2 (* e f)) (+ (* e e) (* f f)))
  )
