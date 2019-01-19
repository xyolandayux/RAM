#lang racket
(require "RAM.rkt")

(define (Gen inp state step)
  (define (cont newinp state out)
     (append out (Gen (append newinp (cdr inp)) state step)))
  (step (if (empty? inp) empty (take inp 1)) state cont))

(define start (ram-store (ram-store (ram-store ram 1 0) 0 2) 2 2))
;;0 is length, size of list 
;;1 is sum
;;2 is index,tranverse every time

(define (step inp state cont)
  (cond
    [(empty? inp) empty]
    [(< (ram-fetch state 0)(ram-fetch state 2)) ;;not in list
     (cont
      empty
      (ram-store (ram-store (ram-store (ram-store state (add1(ram-fetch state 0))(car inp)) 0 (add1 (ram-fetch state 0)))1 (+ (ram-fetch state 1) (car inp))) 2 3)
      (list (+ (ram-fetch state 1) (car inp))))]
     [(= (car inp) (ram-fetch state (ram-fetch state 2)))
      (cont empty (ram-store state 2 3)(list (ram-fetch state 1)))]
     [else (cont inp (ram-store state 2 (add1 (ram-fetch state 2))) empty)]))

(define (accum s) (Gen s start step))
