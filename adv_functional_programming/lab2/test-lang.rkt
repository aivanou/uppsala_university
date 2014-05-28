#lang s-exp "relations.rkt"
  
(define x 5)
(+ x 10)
'(1 2 3)

(name)

(define (sum x) (symbol? x))

(sum ultra)

(define-syntax-rule (tr . arg)          
         'arg)

(tr name)


(define (init-col-hash hash cols)
  (let init-next-col  ([cols cols] [iter 0])
    (match cols
      ['() hash]
      [(list first rest ...)
       (hash-set! hash first iter)
       (init-next-col rest (+ 1 iter))])))


(init-col-hash (make-hash) (name surname age))

(define p (make-relation (name surname age)
                         (("alex" "ivanov" 20)
                          ("peter1" "mackonnel1" 32)
                          ("peter2" "mackonnel2" 11)
                          ("peter3" "mackonnel3" 44))))


(table-data p)
(table-col-hash p)

(relation people (make-relation (name surname age)
                         (("alex" "ivanov" 20)
                          ("peter" "mackonnel" 32))))

(cols-numbers (table-col-hash p) (age name))

(project p (age name))
(project people (age name))

(show people)


(define c (and (> a "ars") (= b 300)))



(show (project (restrict p (or (> age 30) (= name "alex"))) (age name)))

(show (restrict-single-entity p (or (> age 30) (not(= name "alex")))))