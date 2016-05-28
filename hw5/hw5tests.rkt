#lang racket

(require "hw5.rkt")

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

(define 2a (equal? (eval-exp (int 1)) (int 1)))
(define 2b (equal? (eval-exp (add (int 1) (int 2))) (int 3)))
(define 2c (equal? (eval-exp (munit)) (munit)))
(define 2d (equal? (eval-exp (isgreater (int 1) (int 0))) (int 1)))
(define 2e (equal? (eval-exp (isgreater (int 0) (int 1))) (int 0)))
(define 2f (equal? (eval-exp (ifnz (int 0) (munit) (int 3))) (int 3)))
(define 2g (equal? (eval-exp (ifnz (int 1) (munit) (int 3))) (munit)))

(define foo (fun "foo" "x" (add (int 1) (int 2))))
(define 2h (equal? (eval-exp (eval-exp foo)) (closure null foo)))

(define mlet1 (mlet "x" (int 1) (add (int 1) (var "x"))))
(define 2i (equal? (eval-exp mlet1) (int 2)))

(define 2j (equal? (eval-exp (call foo (munit))) (int 3)))
(define 2k (equal? (eval-exp (call foo (apair (int 1) (int 2)))) (int 3)))

(define bar (fun "bar" "y" (add (int 1) (var "y"))))
(define 2l (equal? (eval-exp (call bar (int 2))) (int 3)))

(define 2m (equal? (eval-exp (apair (add (int 1) (int 2)) (int 2))) (apair (int 3) (int 2))))

(define pair (apair (add (int 1) (int 2)) (isgreater (int 1) (int 0))))
(define 2n (equal? (eval-exp (first pair)) (int 3)))
(define 2o (equal? (eval-exp (second pair)) (int 1)))

(define 2p (equal? (eval-exp (ismunit (munit))) (int 1)))

(define 3a (equal? (eval-exp (ifmunit (munit) (int 1) (int 2))) (int 1)))
(define 3b (equal? (eval-exp (ifmunit (int 0) (int 1) (int 2))) (int 2)))

(define 3c (equal? (eval-exp (mlet* (list (list "x" (int 1)) (list "y" (int 0))) (add (var "x") (var "y")))) (int 1)))
(define 3d (equal? (eval-exp (ifeq (int 1) (int 1) (int 2) (int 3))) (int 2)))
(define 3e (equal? (eval-exp (ifeq (int 1) (int 2) (int 2) (int 3))) (int 3)))

