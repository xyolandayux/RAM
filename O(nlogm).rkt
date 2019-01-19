#lang racket
(require "RAM.rkt")

(define (Gen inp state step)
  (define (cont newinp state out)
    (append out (Gen (append newinp (cdr inp)) state step)))
  (step (if (empty? inp) empty (take inp 1)) state cont))

(define start (ram-store
               (ram-store
                (ram-store
                 (ram-store
                  (ram-store
                   (ram-store ram 1 0) 0 5) 2 5) 3 0) 4 0) 5 0))
;;0 is length, size of list 
;;1 is sum
;;2 is index,tranverse every time

(define (step inp state cont)
  (cond
    [(empty? inp) empty]
    [(> (ram-fetch state (ram-fetch state 2))(car inp))
     (if (= 0 (ram-fetch state (- (ram-fetch state 2) 2)))
         (cont empty
(ram-store 
               (ram-store ;;add new node to tree
                (ram-store ;;add to length
                 (ram-store ;;add to sum
                  (ram-store ;;store new element
                   (ram-store ;;initialize tree
                    (ram-store ;;initilize tree
                     state
                     (+ (ram-fetch state 0) 1) 0)
                    (+ (ram-fetch state 0) 2) 0)
                   (+ 3 (ram-fetch state 0))(car inp))
                  1 (+ (ram-fetch state 1) (car inp)))
                 0 (+ 3 (ram-fetch state 0)))
                (- (ram-fetch state 2) 2) (+ 3 (ram-fetch state 0)))
               2 5)
               (list (+ (car inp)(ram-fetch state 1))))
         (cont inp (ram-store state 2 (ram-fetch state (- (ram-fetch state 2) 2))) empty))]

    [(< (ram-fetch state (ram-fetch state 2)) (car inp))
     (if (= 0 (ram-fetch state (- (ram-fetch state 2) 1)))
         (cont empty
              (ram-store  (ram-store ;;add new node to tree
                (ram-store ;;add to length
                 (ram-store ;;add to sum
                  (ram-store ;;store new element
                   (ram-store ;;initialize tree
                    (ram-store ;;initilize tree
                     state
                     (+ (ram-fetch state 0) 1) 0)
                    (+ (ram-fetch state 0) 2) 0)
                   (+ 3 (ram-fetch state 0))(car inp))
                  1 (+ (ram-fetch state 1) (car inp)))
                 0 (+ 3 (ram-fetch state 0)))
                (- (ram-fetch state 2) 1) (+ 3 (ram-fetch state 0)))
                          2 5)
               (list (+ (car inp)(ram-fetch state 1))))
         (cont inp (ram-store state 2 (ram-fetch state (- (ram-fetch state 2) 1))) empty))]

    [(= (car inp) (ram-fetch state (ram-fetch state 2)))
     (cont empty (ram-store state 2 5) (list (ram-fetch state 1)))]))

(define (accum s) (Gen s start step))
