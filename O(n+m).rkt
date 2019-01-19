#lang racket
(require "RAM.rkt")

(define (Gen inp state step)
  (define (cont newinp state out)
     (append out (Gen (append newinp (cdr inp)) state step)))
  (step (if (empty? inp) empty (take inp 1)) state cont))

(define start (ram-store (ram-store ram 1 0) 0 0))
;;0 is length, size of list 
;;1 is sum

(define (step inp state cont)
  (cond
    [(empty? inp) empty]
    [(<= (ram-fetch state 0)(car inp)) ;;out of bound, create longer list
     (cont
      inp
      (ram-store
       (ram-store state 0 (add1 (ram-fetch state 0)))
       (+ 2 (ram-fetch state 0))
       0) empty)]
    [else (if (= 0 (ram-fetch state (+ 2 (car inp))))
              (cont
               empty
               (ram-store (ram-store state (+ 2 (car inp)) 1) 1 (+ (car inp)(ram-fetch state 1)))
               (list (+ (car inp)(ram-fetch state 1))))
              (cont empty state (list (ram-fetch state 1))))]))

(define (accum s) (Gen s start step))