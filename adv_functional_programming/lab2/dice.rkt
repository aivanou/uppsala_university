#lang racket
(module graph racket
(require racket/include)
  (provide new direct_graph direct_graph-vertex_table direct_graph-vertex_amount
           )
  (include "contracts.rkt") 
 
  (define (has_vertex? digraph a)
    (hash-has-key? (direct_graph-vertex_table digraph) a))                                        
  
  (struct direct_graph (vertex_table vertex_amount))  
   
  (define new ( direct_graph (hash) 0))

  (define (add_vertex digraph a)
    (cond [(hash-has-key? (direct_graph-vertex_table digraph) a) error "already have such vertex"]
          [else (direct_graph (hash-set (direct_graph-vertex_table digraph) a '() ) 0 )]))
  
  (define (add_edge digraph a b w)
    (direct_graph (hash-set (direct_graph-vertex_table digraph) a (cons (list b w) (hash-ref (direct_graph-vertex_table digraph) a))) 0 ))
   
  (define (has_edge? digraph a b)
    (and (has_vertex? digraph a)  ((lambda(lst v) 
                                     (let check_pair ([lst lst])
                                       (match lst
                                         ['() #f]
                                         [(list (list a b) c ...) 
                                          (cond [(= a v) #t]
                                                [else (check_pair c)])]))) (hash-ref (direct_graph-vertex_table digraph) a) b)))
  
  (define (out_neighbours digraph a)
    (match (hash-ref (direct_graph-vertex_table digraph) a)
      ['() '()]
      [(list (list v w) ...) v]))
  
  (define (weight digraph a b)
    ((lambda(lst v)
     (let get_value ([lst lst])
       (match lst
         ['() error "there is no edge"]
         [(list (list a b) c ...)
          (cond [(= a v) b]
                [else (get_value c)])]))) (hash-ref (direct_graph-vertex_table digraph) a) b))
  
)

(require 'graph) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dice Task 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (has-element? lst el)
  (match lst
    ['() #f]
    [(list head rest ...)
     (cond [(= head el) #t]
           [else (has-element? rest el)])]))

(define (shrink-list lst)
  (let shrink-elements([lst (sort lst <)] [out-lst '()] [last-out-el -1])
    (match lst
      ['() out-lst]
      [(list el '()) 
       (cond [(= el last-out-el) out-lst]
             [else (cons el out-lst)])]
      [(list el rest ...) 
       (cond [(= el last-out-el) (shrink-elements rest out-lst last-out-el)]
             [else (shrink-elements rest (cons el out-lst) el)])])))

(define (equal-lists? lst1 lst2)
  (let check-next-el ([lst1 (sort lst1 <=)] [lst2 (sort lst2 <=)])
    (match (list lst1 lst2)
      [(list '() '()) #t]
      [(list '() (list a rest ...)) #f]
      [(list (list a rest ...) '()) #f]
      [(list (list el1 rest1 ...) (list el2 rest2 ...))
       (cond ((= el1 el2) (check-next-el rest1 rest2))
             (else #f))])))

(define (has-list? lists check_list)
  (match lists
    ['() #f]
    [(list el rest ...) 
     (cond [(equal-lists? el check_list) #t]
           [else (has-list? rest check_list)])]))


(define (make-step dg vertex-lst)
  (let proc-next-vertex ([lst vertex-lst] [out-lst '()])
    (match lst
      ['() out-lst]
      [(list vertex rest ...) (proc-next-vertex rest (append (out_neighbours dg vertex) out-lst))])))


(define (make-graph-vertex nvertex)
  (let create-next-vertex ( [dg (direct_graph (hash) nvertex)] [curr-vertex 1])
    (cond [(= curr-vertex (+ 1 nvertex)) dg]
          [else (create-next-vertex (direct_graph (hash-set (direct_graph-vertex_table dg) curr-vertex '()) nvertex) (+ 1 curr-vertex))])))
  

(define (make-graph-edges dg edges)
  (let create-next-edge([edges edges] [dg dg])
    (match edges
      ['() dg]
      ((list (list v-from v-to) rest ...) (create-next-edge rest (add_edge dg v-from v-to 0))))))

(define (make-graph nvertex edges)
  (make-graph-edges (make-graph-vertex nvertex) edges))

(define (make-move dg dice-value current-vertexes)
  (let make-next-step ([verts current-vertexes] [cur-dice-value 1])
    (cond [(= cur-dice-value (+ 1 dice-value)) verts]
          [(empty? verts) '()]
          [else (make-next-step (shrink-list (make-step dg verts)) (+ 1 cur-dice-value))])))

;;Breath-first search
;;When we reach the end of dice list we remember current set of vertices
;;Then if we reach the end of dice list and our current set of vertices the same as one of the set that we reached before
;; we say that there is a cycle and return -1

(define (dice-process start-node win-node dice-list dg)
  (let move([depth 0] [curr-verts (list start-node)] [curr-dice-list dice-list] [border-lists '()])
    (cond [(empty? curr-verts) -1]
          [(has-element? curr-verts win-node) depth]
          [else
           (match curr-dice-list
             ['() (cond [(has-list? border-lists curr-verts) -1]
                        [else (move depth curr-verts dice-list (cons curr-verts border-lists))])]           
             [(list dice-value rest ...)
              (move (+ 1 depth) (make-move dg dice-value curr-verts) rest border-lists)])])))

(define (dice nvertex edges dice-list)
  (dice-process 1 nvertex dice-list (make-graph nvertex edges)))


(displayln (list '(1 2) '(1 3) '(4 5) '(7 4 ) '(2 1) '(4 4)))


(direct_graph-vertex_table(make-graph-edges (make-graph-vertex 10) '((1 2) (1 3) (4 5) (7 4 ) (2 1) (4 4))))

(dice 4 '((1 2) (2 3) (3 4)) '(1))

(dice 6 '((1 2) (1 4) (1 3) (1 5) (2 3) (3 6) (3 1) (4 5) (5 1)(5 2)(5 6)) '(2 4 2))

(dice 3 '((1 2) (2 3)) '(4 2 6))

(dice 6 '((1 2) (2 3) (3 2) (3 4) (4 3) (4 5) (5 4) (5 6) (6 5)) '(2 4 6))
                              
