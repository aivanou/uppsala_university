
(provide 
 (contract-out
  [add_vertex (->i ([dg any/c]
                    [v (dg) (and/c integer? (lambda(vt)
                                              (not(has_vertex? dg vt))))])
                   any)]
  
  [add_edge (->i ([dg any/c]
                  [v1 (dg) (is_vertex? dg)]
                  [v2 (dg v1) (and/c (is_vertex? dg) (lambda(vt)
                                                       (not(has_edge? dg v1 vt))))]
                  [w integer?])
                 any)]
  [has_vertex? (->i ([dg any/c]
                     [v (dg) integer?])
                    [result boolean?])]
  
  [has_edge? (->i ([dg any/c]
                   [v1 (dg) (is_vertex? dg)]
                   [v2 (dg) (is_vertex? dg)])
                  [result boolean?])]
  
  [out_neighbours (->i ([dg any/c]
                        [v (dg) (is_vertex? dg)])
                       [result (dg v) (and/c list? (lambda(lst)
                                                     (let check_vertex([lst lst])
                                                       (match lst 
                                                         ['() #t]
                                                         [(list head rest ...) 
                                                          (cond [(has_vertex? dg head) (check_vertex rest)]
                                                                [else #f])]))))])]
  
  [weight (->i ([dg any/c]
                [v1 (dg) (is_vertex? dg)]
                [v2 (dg v1) (is_edge? dg v1)])
               [res integer?])]))


(define ((is_vertex? dg) v)
  (and (integer? v) (has_vertex? dg v)))

(define ((is_edge? dg src) dest)
  (and (and (is_vertex? src) (is_vertex? dest))
       (has_edge? dg src dest)))