#lang racket

(require "hw4.rkt")
;; Tracy Tran

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here

; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define nums (sequence 1 0 5))

(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))

(define funny-test (stream-for-k-steps funny-number-stream 16))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-one-only)
  (place-repeatedly (open-window) 0.5 (stream-add-one dan-then-dog) 27))

;_________________________________________

; problem 1 tests
(define 1a (equal? (sequence 2 3 11) '(3 5 7 9 11)))
(define 1b (equal? (sequence 3 3 8) '(3 6)))
(define 1c (equal? (sequence 1 3 2) null))

; problem 2 tests
(define xs (list "hi" "bye" "yay"))
(define 2a (equal? (string-append-map xs "hi") (list "hihi" "byehi" "yayhi") ))
(define 2b (equal? (string-append-map null "hi") null))

; problem 3 tests
(define ys (list 0 1 2 3 4 5 6 7 8 9 10))
(define 3a (equal? (list-nth-mod ys 14) 3))
(define 3b (equal? (list-nth-mod ys 11) 0))
(define 3c (equal? (list-nth-mod ys 7) 7))
(define 3d (equal? (list-nth-mod ys 11) 0))

; problem 4 tests
(define naturals
  (letrec ([helper (lambda(x) (cons x (lambda() (helper (+ x 1)))))])
    (lambda() (helper 1))))

(define 4a (equal? (stream-for-k-steps naturals 5) (list 1 2 3 4 5)))
(define 4b (equal? (stream-for-k-steps naturals 1) (list 1)))
(define 4c (equal? (stream-for-k-steps naturals 0) null))

; problem 5 tests
(define 5a (equal? (stream-for-k-steps funny-number-stream 15) (list 1 2 3 4 5 -6 7 8 9 10 11 -12 13 14 15)))
(define 5b (equal? (stream-for-k-steps funny-number-stream 6) (list 1 2 3 4 5 -6)))

; problem 6 tests
(define 6a (equal? (stream-for-k-steps dan-then-dog 4) (list "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg")))
(define 6b (equal? (stream-for-k-steps dan-then-dog 3) (list "dan.jpg" "dog.jpg" "dan.jpg")))

; problem 7 tests
(define 7a (equal? (stream-for-k-steps (stream-add-one naturals) 4) (list (cons 1 1) (cons 1 2) (cons 1 3) (cons 1 4))))
(define 7b (equal? (stream-for-k-steps (stream-add-one dan-then-dog) 3) (list (cons 1 "dan.jpg") (cons 1 "dog.jpg") (cons 1 "dan.jpg"))))

; problem 8 tests
(define 8a (equal? (stream-for-k-steps (cycle-lists xs ys) 15) (list (cons "hi" 0) (cons "bye" 1) (cons "yay" 2)
                                                                     (cons "hi" 3) (cons "bye" 4) (cons "yay" 5)
                                                                     (cons "hi" 6) (cons "bye" 7) (cons "yay" 8)
                                                                     (cons "hi" 9) (cons "bye" 10) (cons "yay" 0)
                                                                     (cons "hi" 1) (cons "bye" 2) (cons "yay" 3))))

; problem 9 tests
(define vec1 (vector (cons 1 null) 4 #t #f 75 (list 1 2) (cons 2 "bye") (cons 3 "dog")))
(define 9a (equal? (vector-assoc 1 vec1)(cons 1 null)))
(define 9b (equal? (vector-assoc 4 vec1) #f))
(define 9c (equal? (vector-assoc #f vec1) #f))
(define 9d (equal? (vector-assoc 3 vec1) "dog"))

; problem 10 tests
(define xs1 (list (cons 1 null) (cons 4 #t) (cons #f 75) (cons 75 (list 1 2 3)) (cons 2 "bye") (cons 3 "dog")))
(define 10a (equal? ((caching-assoc xs1 5) 75) (list 75 1 2 3)))
(define 10b (equal? ((caching-assoc xs1 5) 1) (list 1)))
(define 10c (equal? ((caching-assoc xs1 3) "dog") #f))
(define 10d (equal? ((caching-assoc xs1 10) 2) (cons 2 "bye")))

; problem 11 tests
(define a 7)
(while-greater 2 do (begin (set! a (- a 1)) (print "x") a))
(while-greater 2 do (begin (set! a (- a 1)) (print "x") a))
(define 11a (equal? a 1))

; test "driver"
(define driver (list (cons "1a" 1a) (cons "1b" 1b) (cons "1c" 1c)
                     (cons "2a" 2a) (cons "2b" 2b)
                     (cons "3a" 3a) (cons "3b" 3b) (cons "3c" 3c) (cons "3d" 3d)
                     (cons "4a" 4a) (cons "4b" 4b) (cons "4c" 4c)
                     (cons "5a" 5a) (cons "5b" 5b)
                     (cons "6a" 6a) (cons "6b" 6b)
                     (cons "7a" 7a) (cons "7b" 7b)
                     (cons "8a" 8a)
                     (cons "9a" 9a) (cons "9b" 9b) (cons "9c" 9c) (cons "9d" 9d)
                     (cons "10a" 10a) (cons "10b" 10b) (cons "10c" 10c) (cons "10d" 10d)
                     (cons "11a" 11a)))
  