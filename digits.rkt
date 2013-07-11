;; This provides base support for the image rendering of mock data 

#lang racket

(require slideshow/pict racket/draw)
(require (planet neil/csv:2:0) net/url)
(require json)

;; This is the loading style for URL data but isn't immediately used
;; in this example 
(define galapagos-url 
  (string->url
   "http://www.stat.washington.edu/~handcock/536/Data/galapagos.csv"))

(define make-galapagos-csv-reader
  (make-csv-reader-maker
   '((separator-chars              #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

;; The core data set associated with images
(define next-row
  ((make-csv-reader-maker '()) (open-input-file "data/digitscheck.csv")))


;; The first row of the data set just shows the label for the digit
;; and the pixel locations as 0 - 255

(define digits-header
  (next-row))

;; For the initial data we'll be looking at something like the number
;; 8
(define digit-row
  (next-row))

(define digit-label 8)

;; Extract the data associated with this number:
;; 8,
;; 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,56,180,255,254,224,116,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,105,233,250,180,120,157,211,5,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,0,0,80,250,228,44,0,98,110,194,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,0,48,247,249,54,34,177,229,254,240,60,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,5,159,254,115,102,239,240,91,39,83,46,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,0,151,254,189,254,231,58,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,9,229,254,254,222,47,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,1,198,254,217,55,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,11,102,254,254,115,0,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,87,250,254,254,120,0,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,93,254,178,49,234,215,28,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,18,219,186,20,0,89,254,120,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,145,231,16,0,0,75,254,120,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,35,221,80,0,0,0,75,255,120,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,9,221,187,0,0,0,0,98,254,76,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,69,254,105,0,0,0,0,137,252,40,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,90,254,87,0,0,4,130,249,145,0,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,113,240,21,0,53,188,251,150,5,0,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,126,240,127,195,243,238,106,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,73,236,254,246,161,68,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
;; 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0


(define digit-pixels)
(quote
 (write-json (list (next-row)))
 )

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





