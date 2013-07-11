;; See the associated guards in Little Schemer 
;; Review the cond arguments 
(define (remove-dups l)
  (cond
   [(empty? l) empty]
   [(empty? (rest l)) l]
   [else
    (let ([i (first l)])
      (if (equal? i (first (rest l)))
          (remove-dups (rest l))
          (cons i (remove-dups (rest l)))))]))
 
 (remove-dups (list "a" "b" "b" "b" "c" "c"))

