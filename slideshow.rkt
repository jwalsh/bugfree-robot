#lang racket slideshow


;; Examples from the Racket documentation 
(require slideshow/pict racket/draw)

5
"art gallery"

(circle 10)
(rectangle 10 20)

(hc-append (circle 10) (rectangle 10 20))

(define c (circle 10))
(define r (rectangle 10 20))

(define (square n)
  (filled-rectangle n n))

(square 10)

(define (four p)
  (define two-p
    (hc-append p p))
  (vc-append two-p two-p))

(four (circle 10))

(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(checker (colorize (square 10) "red")
         (colorize (square 10) "black"))

(define (checkerboard p)
  (let* ([rp (colorize (square p) "red")]
         [bp (colorize (square p) "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))

(checkerboard 20)

(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 15)))

(series circle)

(series (lambda (r) (colorize (circle r) "white")))

(series (lambda (s) (checkerboard s)))





