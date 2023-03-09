#lang racket

;; Schema acestui BFS simplificat este:
;;  1. inițializăm coada de noduri care trebuie vizitate cu
;;     rădăcina arborelui (tripletul (3,4,5))
;;  2. adăugăm primul nod din coadă în rezultat
;;  3. adăugăm cei 3 succesori ai săi în coada de noduri
;;     care trebuie vizitate
;;  4. revenim la pasul 2 (întrucât construim un flux
;;     infinit, nu există condiție de oprire, și toate
;;     structurile sunt fluxuri: atât coada cât și
;;     rezultatul funcției BFS)

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


; TODO
; Aduceți aici (nu sunt necesare modificări) implementările
; funcțiilor dot-product și multiply din etapa 1 sau 2.
; Cele două funcții nu sunt re-punctate de checker, însă 
; sunt necesare generării succesorilor unui nod.
(define (dot-product X Y)
  (apply + (map * X Y)))

(define (multiply M V)
  (map (lambda(x) (dot-product x V)) M)
  )


; TODO
; Definiți fluxul infinit de TPP folosind algoritmul descris
; (parcurgerea BFS a arborelui infinit).
; Funcție utilă: stream-append
; Folosiți cel puțin o formă de let.

(define (BFS-traversal2 queue triplet)
  (define triplet1 (multiply T1 triplet))
  (define triplet2 (multiply T2 triplet))
  (define triplet3 (multiply T3 triplet))

  ; (stream-append (list (car queue)) (BFS-traversal (cdr (append queue (list triplet1 triplet2 triplet3))) (cadr (append queue (list triplet1 triplet2 triplet3)))))

  (stream-cons (car queue) (BFS-traversal2 (cdr (append queue (list triplet1 triplet2 triplet3))) (cadr (append queue (list triplet1 triplet2 triplet3)))))
  )

(define (BFS-traversal queue triplet)
  (let ((triplet1 (multiply T1 triplet)) (triplet2 (multiply T2 triplet)) (triplet3 (multiply T3 triplet)))
    (stream-append (stream (car queue)) (stream-lazy (let ((queue (append queue (list triplet1 triplet2 triplet3)))) (BFS-traversal (cdr queue) (cadr queue)))))))

; (let ((queue (append queue (list triplet1 triplet2 triplet3)))) (BFS-traversal (cdr queue) (cadr queue)))

(define ppt-stream-in-tree-order
  (BFS-traversal '((3 4 5)) '(3 4 5))
  )

 (stream->list (stream-take ppt-stream-in-tree-order 10))

(define (naturals-from n)
  (stream-cons n (naturals-from (+ n 1))))

; (stream->list (stream-take (naturals-from 1) 10))