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

   (check-equal? (eval-exp (isgreater (int 1) (int 2))) (int 0) "check is greater false dir")
   
   (check-equal? (eval-exp (isgreater (int 3) (int 2))) (int 1) "check is greater true dir")
   
   (check-equal? (eval-exp (isgreater (add (int 1) (int 2)) (add (int 0) (int 1)))) (int 1) "check is greater complex")
   
   (check-exn (lambda (x) (string=? (exn-message x) "MUPL isgreater applied to non-number"))
              (lambda () (eval-exp (isgreater (int 2) (munit))))
              "isgreater bad argument")
   
   (check-equal? (eval-exp (ifnz (int 1) (int 5) (int 6))) (int 5) "check ifnz1")

   (check-equal? (eval-exp (ifnz (int 0) (int 5) (int 6))) (int 6) "check ifnz2")

   (check-equal? (eval-exp (ifnz (int 1) (munit) (int 6))) (munit) "check ifnz3 non-number es for eval")

   ;(check-exn (lambda (x) (string=? (exn-message x) "MUPL ifnz argument 1 evaluates to a non-number"))
    ;          (lambda () (eval-exp (ifnz (munit) (munit) (int 1))))
     ;         "ifz bad argument")

   (check-equal? (eval-exp (mlet "x" (int 5) (add (var "x") (int 11)))) (int 16) "check mlet")

   (check-equal? (eval-exp (mlet "x" (munit) (var "x"))) (munit) "check mlet non-num")

   (check-equal? (eval-exp (call (fun "fun" "x" (add (var "x") (int 5))) (int 5))) (int 10) "check call")

   (check-equal? (eval-exp (call (fun "fun" "x" (ifnz (var "x")
                                                      (call (var "fun") (add (var "x") (int -1)))
                                                      (int 0))) (int 5))) (int 0) "check call recursive")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL call applied to non-closure"))
              (lambda () (eval-exp (call (int 1) (int 4))))
              "call bad argument")

   (check-equal? (eval-exp (apair (add (int 1) (int 4)) (add (int 6) (int 5)))) (apair (int 5) (int 11)) "check apair")

   (check-equal? (eval-exp (apair (add (int 1) (int 4)) (munit))) (apair (int 5) (munit)) "check apair non-num arg")

   (check-equal? (eval-exp (first (apair (int 1) (int 2)))) (int 1) "first test")

   (check-equal? (eval-exp (second (apair (int 1) (int 2)))) (int 2) "second test")

   ;(check-exn (lambda (x) (string=? (exn-message x) "MUPL first applied to a non-pair"))
    ;          (lambda () (eval-exp (first (int 1))))
     ;         "first bad argument")

   ;(check-exn (lambda (x) (string=? (exn-message x) "MUPL second applied to a non-pair"))
    ;          (lambda () (eval-exp (second (int 1))))
     ;         "second bad argument")
   
   (check-equal? (eval-exp (ismunit (apair (int 1) (int 2)))) (int 0) "ismunit test1")

   (check-equal? (eval-exp (ismunit (munit))) (int 1) "ismunit test2")

   (check-equal? (eval-exp (ifmunit (munit) (int 1) (int 0))) (int 1) "ifmunit test1")

   (check-equal? (eval-exp (ifmunit (int 6) (int 1) (int 0))) (int 0) "ifmunit test2")

   (check-equal? (eval-exp (mlet* (list (cons "x" (int 1)) (cons "y" (int 2)) (cons "z" (int 3)))
                                 (add (var "x") (add (var "y") (var "z"))))) (int 6) "mlet test")

   (check-equal? (eval-exp (ifeq (int 6) (int 6) (int 1) (int 0))) (int 1) "ifmunit test2")
   
   (check-equal? (eval-exp (ifeq (int 6) (int 7) (int 1) (int 0))) (int 0) "ifmunit test2")

   (check-equal? (eval-exp (call (eval-exp
                                  (call mupl-filter (fun "test" "x" (isgreater (var "x") (int 0)))))
                                 (apair (int 0) (apair (int 2) (munit))))) (apair (int 2) (munit)) "filter test")

   (check-equal? (eval-exp
                  (call (eval-exp (call mupl-all-gt (int 3)))
                        (apair (int 1) (apair (int 3) (apair (int 5) (munit)))))) (apair (int 5) (munit)) "all-gt test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
