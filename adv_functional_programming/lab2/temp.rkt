#lang racket

(module counter racket
  (struct counter (cnt))
  
  (define (count) (counter 0))
  
  (define (inc cnt) (counter (add1 (counter-cnt cnt))))
  
  (define (val cnt) (counter-cnt cnt))
  
  (provide
   (contract-out
    [count (-> (and/c counter? (lambda (res) (= 0 (val res)))))]
    [val (-> counter? natural-number/c)]
    [inc (->i ([cnt counter?])
              [res (cnt)
                   (and/c counter?
                          (lambda (rr)
                            (= (val rr) (add1 (val cnt)))))])]))
  
)

(require 'counter)
(define x (count))
(val x)
(define y (inc (inc x)))
(val y)

(module tt racket
  (require racket/include)
  (include "contracts.rkt")
  
  (provide
   (contract-out
    [minus (->i ([arg1 natural-number/c]
                 [arg2 (arg1) (and/c natural-number/c (<=/c arg1))]
                 [arg3 natural-number/c])
                any)]))
  
  (define (fact x) (* x x))
  
  (define (minus a b c) (- a b))
  
)

(require 'tt)

(minus 100 40 111)

(fact 100)

(struct point (x y))
(struct dpraph(vertexList edgeList ))




(define xp1 (point 1 2))
(define xp2 (point 32 32))

(define d (dpraph (list xp1 xp2) (list 3 4 3 2 2)))

(dpraph-vertexList d)

(define (tt lst)
  (let length-help ([lst lst] [len 0])
    (match lst
      ['() len]
      [(list el) (+ len 1)]
      [(list head rest ... ) (length-help rest (+ len 1))])))


(define (f num . nums) nums)

(f 1 2 3 4 5 6 7)
  

(define color (make-parameter "Blue"))
(define (best-color) (displayln (color)))
(best-color)
(parameterize ([color "Red"]) (best-color))
(best-color)


(+ 2 (call/cc (lambda (cc) (+ 310 (cc 10)))))

(define sc #f)
(define (save! x)
(call/cc (lambda (cc) (set! sc cc) x)))

(+ 1 (+ (+ (save! 3) 4) 2))
(sc 10)
(sc 20)

(define (current-continuation)
  (call/cc (lambda (cc) cc)))

(define (choose a b)
  (let ([cc (current-continuation)])
    (cond [(continuation? cc) (values a cc)]
          [else (values b #f)])))
(define (fail cc)
  (when (continuation? cc) (cc #f)))

(define (run)
  (define-values (val cc) (choose 3 1))
  (displayln val)
  (fail cc))


(run)






#lang racket

(provide + - / * = > < >= <= if
         #%app 
         #%datum
         #%module-begin
         #%top-interaction
         (rename-out [my-define define]
                     [lookup #%top])
         sum
         )


(define env (make-hash))
(define-syntax-rule (my-define id expr)
  (let ([val expr])
    (hash-set! env 'id val)
    (void)))

(define-syntax-rule (lookup . id)
  (if (hash-has-key? env 'id)
      (let* ([val (hash-ref env 'id)]
             [new-val (floor (/ val 2))])
        (if (not (= new-val 0))
            (hash-set! env 'id new-val)
            (hash-remove! env 'id))
        val)
      (error "This id does not exist (anymore)")))

(define (sum x y) (+ x y))