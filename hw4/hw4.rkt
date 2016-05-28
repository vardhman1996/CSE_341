#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

; Vardhman Mehta
; 1428251
; mehtav@uw.edu
;; put your code below

; Question 1
(define (sequence spacing low high)
  (if (> low high)
      null
      (cons low (sequence spacing (+ low spacing) high))))

; Question 2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

; Question 3
(define (list-nth-mod xs n)
  (cond [(negative? n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; Question 4
(define (stream-for-k-steps s k)
  (if (= k 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-k-steps (cdr pr) (- k 1))))))

; Question 5
(define funny-number-stream
  (letrec ([negate (lambda (x) (if (zero? (remainder x 6)) (- x) x))]
           [f (lambda (x) (cons (negate x) (lambda () (f (+ x 1)))))])
  (lambda () (f 1))))

; Question 6
(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    dan))

; Question 7
(define (stream-add-one s)
  (letrec ([f (lambda (x) (cons (cons 1 (car x)) (lambda () (f ((cdr x))))))])
    (lambda () (f (s)))))

; Question 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (x)
                (cons
                 (cons (list-nth-mod xs x) (list-nth-mod ys x))
                 (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

; Question 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (idx)
                (if (>= idx (vector-length vec))
                    #f
                    (let ([pr (vector-ref vec idx)])
                      (if (and (pair? pr) (equal? v (car pr)))
                          pr
                          (f (+ idx 1))))))])
    (f 0)))

; Question 10
(define (caching-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [pos 0])
    (lambda (v)
      (or (vector-assoc v cache)
          (let ([newans (assoc v xs)])
            (and newans (begin
                       (vector-set! cache pos newans)
                       (set! pos (remainder (+ pos 1) n))
                        newans)))))))

; Question 11
(define-syntax while-greater
  (syntax-rules (do)
    [(while-greater e1 do e2)
     (letrec ([first e1]
              [f (lambda ()
                   (if (<= e2 first)
                       #t
                       (f)))])
       (f))]))

; Question 12
(define (cycle-lists-challenge xs ys)
  (define (xsOld xs)
    (letrec ([f (lambda (x)
                  (if (pair? (cdr x))
                      (cons (car x) (lambda () (f (cdr x))))
                      (cons (car x) (lambda () (f xs)))))])
      (lambda() (f xs))))
  (letrec ([xsStream (xsOld xs)]
           [ysStream (xsOld ys)]
           [f (lambda (x y)
                (cons (cons (car (x)) (car (y)))
                      (lambda () (f (cdr (x)) (cdr (y))))))])
    (lambda () (f xsStream ysStream))))