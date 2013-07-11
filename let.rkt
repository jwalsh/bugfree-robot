;; While let makes its bindings available only in the bodys, and let*
;; makes its bindings available to any later binding expr, letrec
;; makes its bindings available to all other exprs—even earlier ones.
;; In other words, letrec bindings are recursive.

;; reusing local binding 
(let* ([x (list "Borroughs")]
       [y (cons "Rice" x)]
       [z (cons "Edgar" y)])
  (list x y z))

;; let bindings in the definition 
(let ([+ (lambda (x y)
           (if (string? x)
               (string-append x y)
               (+ x y)))]) ; use original +
  (list (+ 1 2)
        (+ "see" "saw")))

(let ([me "Tarzan"]
      [you "Jane"])
  (let ([me you]
        [you me])
    (list me you)))


(define make-swing ()
  (letrec ([swing
            (lambda (t)
              (if (eq? (car t) 'tarzan)
                  (cons 'vine
                        (cons 'tarzan (cddr t)))
                  (cons (car t)
                        (swing (cdr t)))))])
    (swing '(vine tarzan vine vine))))

(letrec ([tarzan-in-tree?
          (lambda (name path)
            (or (equal? name "tarzan")
                (and (directory-exists? path)
                     (tarzan-in-directory? path))))]
         [tarzan-in-directory?
          (lambda (dir)
            (ormap (lambda (elem)
                     (tarzan-in-tree? (path-element->string elem)
                                      (build-path dir elem)))
                   (directory-list dir)))])
  (tarzan-in-tree? "tmp" (find-system-path 'temp-dir)))

(define (duplicate pos lst)
  (let dup ([i 0]
            [lst lst])
    (cond
     [(= i pos) (cons (car lst) lst)]
     [else (cons (car lst) (dup (+ i 1) (cdr lst)))])))

(duplicate 1 (list "apple" "cheese burger!" "banana"))
