#lang racket

(require "hw5.rkt")
;; Tracy Tran


; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Homework 5 Tests"

   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")
   (check-exn (lambda (x) (string=? (exn-message x) "unbound variable during evaluation \"hi\""))
              (lambda () (eval-exp (first (var "hi")))))

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL isgreater applied to non-number"))
              (lambda () (eval-exp (isgreater (munit) (int 3)))))

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL isgreater applied to non-number"))
              (lambda () (eval-exp (isgreater (int 3) (munit)))))

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL ifnz applied to non-number"))
              (lambda () (eval-exp (ifnz (munit) (int 5) (apair (munit) (int 4))))))

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL call applied to non-closure"))
              (lambda () (eval-exp (call (apair (int 4) (int 2)) (int 1)))))

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL first applied to non-apair"))
              (lambda () (eval-exp (first (isgreater (int 3) (int 4))))))

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL second applied to non-apair"))
              (lambda () (eval-exp (second (isgreater (int 3) (int 4))))))
   

   (check-equal? (mupllist->racketlist
                 (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)

;_________________________________________

; problem 1 tests
(define xs (list 1 2 3 4 5))
(define xs2 null)
(define xs3 (list 2))
(define ys (racketlist->mupllist xs))
(define ys2 (racketlist->mupllist xs2))
(define ys3 (racketlist->mupllist xs3))
(define zs (mupllist->racketlist ys))
(define zs2 (mupllist->racketlist ys2))
(define zs3 (mupllist->racketlist ys3))
(define 1a (equal? ys (apair 1 (apair 2 (apair 3 (apair 4 (apair 5 (munit))))))))
(define 1b (equal? ys2 (munit)))
(define 1c (equal? ys3 (apair 2 (munit))))
(define 1d (equal? zs xs))
(define 1e (equal? zs2 xs2))
(define 1f (equal? zs3 xs3))

  
; problem 2 tests
(define 2a (equal? (eval-exp (int 17)) (int 17))) ; int

(define 2b (equal? (eval-exp (munit)) (munit))) ; munit

(define 2c (equal? (eval-exp (add (int 5) (int 2))) (int 7))) ; add

(define 2d (equal? (eval-exp (isgreater (int 7) (int 2))) (int 1))) ; isgreater
(define 2e (equal? (eval-exp (isgreater (int 2) (int 7))) (int 0))) ; isgreater

(define 2f (equal? (eval-exp (apair (int 7) (munit))) (apair (int 7) (munit)))) ; apair
(define 2fa (equal? (eval-exp (apair (int 7) (first (apair (second (apair (int 7) (int 14))) (munit))))) (apair (int 7) (int 14)))); complicated apair

(define 2g (equal? (eval-exp (first (apair (int 7) (munit)))) (int 7))) ; first
(define 2h (equal? (eval-exp (second (apair (int 7) (munit)))) (munit))) ; second

(define add-one (fun "add-one" "x" (add (var "x") (int 1))))
(define 2i (equal? (eval-exp (eval-exp add-one)) (closure null add-one))) ; closure

(define 2j (equal? (eval-exp (ifnz (int 0) (munit) (int 18))) (int 18))) ; ifnz
(define 2ja (equal? (eval-exp (ifnz (int 1) (munit) (int 18))) (munit))) ; ifnz

(define dumb-fun (fun "dumb-fun" "x" (add (int 4) (int 5))))
(define 2k (equal? (eval-exp add-one) (closure null add-one))) ; fun
(define 2ka (equal? (eval-exp dumb-fun) (closure null dumb-fun))) ; fun

(define mlet1 (mlet "x" (first (apair (int 3) (int 2))) (add (int 1) (var "x"))))
(define mlet2 (mlet "x" (ifnz (int 1) (munit) (int 3)) (var "x")))
(define 2l (equal? (eval-exp mlet1) (int 4))) ; mlet
(define 2la (equal? (eval-exp mlet2) (munit))) ; mlet

(define 2m (equal? (eval-exp (call dumb-fun (munit))) (int 9))) ; call
(define 2ma (equal? (eval-exp (call dumb-fun (apair (int 2) (munit)))) (int 9))) ; call
(define 2mb (equal? (eval-exp (call add-one (int 3))) (int 4))) ; call

(define 2n (equal? (eval-exp (ismunit (munit))) (int 1))) ; ismunit
(define 2na (equal? (eval-exp (ismunit (apair (int 4) (munit)))) (int 0))) ; ismunit
(define 2nb (equal? (eval-exp (ismunit (first (apair (munit) (int 4))))) (int 1))) ; ismunit
(define 2nc (equal? (eval-exp (ismunit (isgreater (int 7) (int 2)))) (int 0))) ; ismunit
                   

; test "driver"
(define driver (list (cons "1a" 1a)
                     (cons "1b" 1b)
                     (cons "1c" 1c)
                     (cons "1d" 1d)
                     (cons "1e" 1e)
                     (cons "1f" 1f)
                     (cons "2a" 2a)
                     (cons "2b" 2b)
                     (cons "2c" 2c)
                     (cons "2d" 2d)
                     (cons "2e" 2e)
                     (cons "2f" 2f)
                     (cons "2fa" 2fa)
                     (cons "2g" 2g)
                     (cons "2h" 2h)
                     (cons "2i" 2i)
                     (cons "2j" 2j)
                     (cons "2ja" 2ja)
                     (cons "2k" 2k)
                     (cons "2ka" 2ka)
                     (cons "2l" 2l)
                     (cons "2la" 2la)
                     (cons "2m" 2m)
                     (cons "2ma" 2ma)
                     (cons "2mb" 2mb)
                     (cons "2n" 2n)
                     (cons "2na" 2na)
                     (cons "2nb" 2nb)
                     (cons "2nc" 2nc)
                     ))

