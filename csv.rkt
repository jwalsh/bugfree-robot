;; Examples from the CSV documentation

#lang racket

(require (planet neil/csv:2:0) net/url)
(require json)

(define galapagos-url 
  (string->url
   "http://www.stat.washington.edu/~handcock/536/Data/galapagos.csv"))

(define make-galapagos-csv-reader
  (make-csv-reader-maker
   '((separator-chars              #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define next-row
  ((make-csv-reader-maker '()) (open-input-file "data/digitscheck.csv")))

(let ([content (next-row)]
      [digit-name (car content)])
  (list digit-name))

(define digits-header
  (next-row))

(quote
 (write-json (list (next-row)))
 )

(define (all-rows url make-reader)
  (define next-row (make-reader (get-pure-port url)))
  (define (loop)
    (define row (next-row))
    (if (empty? row)
        '()
        (cons row (loop))))
  (loop))

(all-rows galapagos-url make-galapagos-csv-reader)
