#lang racket

(provide core-dump ram ram-fetch ram-store)

;; ram is an unbounded RAM
;; (ram-store ram addr b) produces ram with word at address addr replaced by w
;;     addr and w must be non-negative intergers
;; (ram-fetch ram addr) produces the value previously stored at address addr in ram

(define-struct wrapper (w))
(define ram (wrapper empty))
(define (ram-fetch a b)
  (define r (trie-fetch (wrapper-w a) b))
  (if (number? r) r (error "attempt to fetch undefined value at " a)))
(define (ram-store a b c) (make-wrapper (trie-store (wrapper-w a) b (if (and (integer? c) (<= 0 c)) c (error "attempt to store non-Nat value")))))
(define-struct trie (left right val) #:transparent)
(define (t-left t) (if (empty? t) empty (trie-left t)))
(define (t-right t) (if (empty? t) empty (trie-right t)))
(define (t-val t) (if (empty? t) 'undefined (trie-val t)))

(define (trie-fetch br addr)
  ;(printf "~a ~a\n" br addr)
  (cond
    [(zero? addr)(t-val (t-left br))]
    [(= 1 addr)(t-val (t-right br))]
    [(even? addr) (trie-fetch (t-left br) (quotient addr 2))]
    [true (trie-fetch (t-right br) (quotient addr 2))]))
     
(define (trie-store br addr bit)
  (cond
    [(zero? addr) (make-trie (make-trie (t-left (t-left br))(t-right(t-left br)) bit)
                             (t-right br)
                             (t-val br))]
    [(= 1 addr) (make-trie   (t-left br)
                             (make-trie (t-left (t-right br))(t-right(t-right br)) bit)
                             (t-val br))]
    [(even? addr) (make-trie (trie-store (t-left br) (quotient addr 2) bit) (t-right br) (t-val br))]
    [true (make-trie (t-left br) (trie-store (t-right br) (quotient addr 2) bit) (t-val br))]))

(define (core-dump r)
  (define (help a b)
    (cond
      [(> a b) (void)]
      [(number? (ram-fetch r a))
         (printf "[~a]: ~a\n" a (ram-fetch r a))
         (help (add1 a) b)]
      [true (help (add1 a) b)]))
  (printf "------------------\n")
  (help 0 1000)
  (printf "------------------\n"))

;; examples:
;; (define x (ram-store (ram-store ram 1 1) 0 0));  store 1 at address 1
;; (ram-fetch x 0) ; error: attempt to fetch undefined value
;; (ram-fetch x 1) ; 1
;(define xx (ram-store ram 22 1))  ; store 1 at address 22
; (ram-fetch xx 22) ; 1
;; (define xxx (ram-store ram 23 -300)) ; attempt to store invalid value
;; (ram-fetch xx 23) ;  attempt to fetch undefined value
