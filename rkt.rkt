#lang typed/racket

(struct: route ([dest : Integer] [cost : Integer]) #:transparent)

(struct: node ([neighbours : (Listof route)]) #:transparent)

(: str->int (String -> Integer))
(define (str->int str) 
  (define n (string->number str))
  (if n (numerator (inexact->exact (real-part n))) 0))

(: read-places (-> (Vectorof node)))
(define (read-places) 
  (define lines 
    (file->lines "agraph"))
  (define num-lines (str->int (car lines)))
  (define nodes (build-vector num-lines (lambda (n) (node `()))))
  (: loop (Integer -> Boolean))
  (define loop (lambda (i)
                 (define nums (string-split (list-ref (cdr lines) i)))
                 (print i)
                 (define len (length nums))
                 (if (and (> len 2) (> (length lines) (+ i 2))) 
                     (let ([node-id (str->int (list-ref nums 0))]
                           [neighbour (str->int (list-ref nums 1))]
                           [cost (str->int (list-ref nums 2))])
                       (define new-node (node 
                                         (append (node-neighbours (vector-ref nodes node-id)) 
                                                 (list (route neighbour cost)))))
                       (vector-set! nodes node-id new-node)
                       (loop (+ i 1))
                       #f)
                     '#f)))
  (loop 1)
  nodes)