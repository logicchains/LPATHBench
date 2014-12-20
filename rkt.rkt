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
    (let loop ([i : Integer 0])
      (define nums (string-split (list-ref (cdr lines) i)))
      (define len (length nums))
      (when (and (> len 2) (> (length lines) (+ i 2)))
          (let ([node-id (str->int (list-ref nums 0))]
                [neighbour (str->int (list-ref nums 1))]
                [cost (str->int (list-ref nums 2))])
            (define new-node (node
                              (append (node-neighbours (vector-ref nodes node-id))
                                      (list (route neighbour cost)))))
            (vector-set! nodes node-id new-node)
            (loop (+ i 1)))))
    nodes)

  (: get-longest-path ((Vectorof node) Integer (Vectorof Boolean) -> Integer))
  (define (get-longest-path nodes node-id visited)
    (vector-set! visited node-id #t)
    (define sum
      (foldr
       (lambda ([neighbour : route] [max : Integer])
         (if (not (vector-ref visited (route-dest neighbour)))
             (let ([dist (+ (route-cost neighbour) (get-longest-path nodes (route-dest neighbour) visited))])
               (if (> dist max)
                   dist
                   max))
             max))
       0
       (node-neighbours (vector-ref nodes node-id))))
    (vector-set! visited node-id #f)
    sum)

  (define nodes (read-places))
  (define visited : (Vectorof Boolean) (build-vector (vector-length nodes) (lambda (n) #f)))
  (define start (current-inexact-milliseconds))
  (define len (get-longest-path nodes 0 visited))
  (define duration (- (current-inexact-milliseconds) start))
  (printf "~a LANGUAGE Racket ~a\n" len (inexact->exact (floor duration)))	