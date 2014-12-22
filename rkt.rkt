#lang typed/racket
(require (only-in racket/unsafe/ops [unsafe-fx+ +] [unsafe-fxmax max]))

(struct route ([dest : node] [cost : Integer]) #:transparent)
(struct node ([neighbours : (Listof route)] [visited? : Boolean]) #:transparent #:mutable)

(: read-places : -> node)
(define (read-places)
  (define (str->int [str : String])
    (assert (string->number str) exact-integer?))
  (define lines (file->lines "agraph"))
  (define num-lines (str->int (car lines)))
  (define nodes (build-vector num-lines (λ (n) (node '() #f))))
  (for ([3nums (in-list (rest lines))])
    (define nums (map str->int (string-split 3nums)))
    (define node      (vector-ref nodes (first nums)))
    (define neighbour (vector-ref nodes (second nums)))
    (define cost      (third nums))
    (set-node-neighbours! node (cons (route neighbour (assert cost fixnum?))
                                     (node-neighbours node))))
  (vector-ref nodes 0))

(: get-longest-path : node -> Fixnum)
(define (get-longest-path node)
  (set-node-visited?! node #t)
  (begin0
      (for/fold ([best : Fixnum 0])
                ([neighbour (in-list (node-neighbours node))]
                 #:unless (node-visited? (route-dest neighbour)))
        (max best (+ (route-cost neighbour)
                     (get-longest-path (route-dest neighbour)))))
    (set-node-visited?! node #f)))

(define root (read-places))
(define start (current-inexact-milliseconds))
(define len (get-longest-path root))
(define duration (- (current-inexact-milliseconds) start))
(printf "~a LANGUAGE Racket ~a\n" len (inexact->exact (floor duration)))
