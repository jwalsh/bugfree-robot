;; file:///Applications/Racket%20v5.3.5/doc/guide/Lists__Iteration__and_Recursion.html
;; Review the lisp processing 

(define (my-length lst)
  "Length"
  (cond
   [(empty? lst) 0]
   [else (+ 1 (my-length (rest lst)))]))


(my-length '(1 2 3 4 5))

;; Notice the use of empty for the recursive guard 
(define (my-map f lst)
  "Map"
  (cond
   [(empty? lst) empty]
   [else (cons (f (first lst))
               (my-map f (rest lst)))]))

(my-map
 (lambda (e) (number? e))
 (list 1 2 3 "foo" 5 "bar"))
