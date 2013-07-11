(define (twice f v)
  (f (f v)))

;; We have so far referred to definitions of the form (define ‹id›
;; ‹expr›) as “non-function definitions.” This characterization is
;; misleading, because the ‹expr› could be a lambda form, in which case the
;; definition is equivalent to using the “function” definition form.

;; lambda 
(define square 
  (lambda (x) (* x x)))

;; sugar
(define (square^ x) (* x x))

(= 
 (square^ 4)
 (square 4))

(twice sqrt (square 4))

(define lat?
  (lambda (s)
    s))

(define member?
  (lambda (s)
    s))

;; remove the first member 
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) empty)
     (else
      (cond
       ((eq? (car lat) a) (cdr lat))
       (else
        (cons (car lat)
              (rember a (cdr lat)))))))))

(define lat (list "c" "b" "a" "b" "a" "b"))

(rember "a" lat)

;; TODO

(define rember*
  (lambda (s)
    s))


(define one?
  (lambda (a lat)
    lat))
