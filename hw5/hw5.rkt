;; CSE341, Programming Languages, Homework 5
;; Vardhman Mehta
;; 1428251
;; mehtav@uw.edu

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct isgreater (e1 e2)    #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3) #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair   (e1 e2) #:transparent) ;; make a new pair
(struct first   (e)     #:transparent) ;; get first part of a pair
(struct second  (e)     #:transparent) ;; get second part of a pair
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
(define (racketlist->mupllist rlist)
  (if (null? rlist)
      (munit)
      (apair (car rlist) (racketlist->mupllist (cdr rlist)))))
      
(define (mupllist->racketlist mlist)
  (if (munit? mlist)
      null
      (cons (apair-e1 mlist) (mupllist->racketlist (apair-e2 mlist)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (int (+ (int-num v1) (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e] ;; TODO: what to do about munit???!!!!!
        [(munit? e) e]
        [(closure? e) e]
        [(isgreater? e)
         (let ([v1 (eval-under-env (isgreater-e1 e) env)]
               [v2 (eval-under-env (isgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (int 1)
                   (int 0))
               (error "MUPL isgreater applied to non-number")))]
        [(ifnz? e)
         (let ([v (eval-under-env (ifnz-e1 e) env)])
           (if (int? v)
               (if (= (int-num v) 0)
                   (eval-under-env (ifnz-e3 e) env)
                   (eval-under-env (ifnz-e2 e) env))
               (error "MUPL if not zero applied to non-number")))]
        [(fun? e) (closure env e)]
        [(mlet? e)
         (let* ([v (eval-under-env (mlet-e e) env)]
                [new_env (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) new_env))]
        [(call? e)
         (let ([c (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
         (if (closure? c)
             (let* ([c_func (closure-fun c)]
                    [new_env (cons (cons (fun-formal c_func) arg) (closure-env c))]
                    [new_env (if (null? (fun-nameopt c_func))
                                 new_env
                                 (cons (cons (fun-nameopt c_func) c) new_env))])
               (eval-under-env (fun-body c_func) new_env))
             (error "MUPL call applied to non-closure")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(first? e)
         (let ([v (eval-under-env (first-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL first applied to non-apair")))]
        [(second? e)
         (let ([v (eval-under-env (second-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL second applied to non-apair")))]
        [(ismunit? e)
         (let ([v (eval-under-env (ismunit-e e) env)])
           (if (munit? v)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3) (ifnz (ismunit e1) e2 e3))

(define (mlet* bs e2)
  (if (null? bs)
      e2
      (mlet (caar bs) (cdar bs) (mlet* (cdr bs) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2)) (ifnz (isgreater (var "_x") (var "_y"))
                                                    e4
                                                    (ifnz (isgreater (var "_y") (var "_x")) e4 e3))))

;; Problem 4


(define mupl-filter
  (fun null "filter-func"
       (fun "mupl-filter" "lst"
            (ifmunit (var "lst")
                     (munit)
                     (mlet "x" (first (var "lst"))
                           (ifeq (call (var "filter-func") (var "x"))
                                 (int 0)
                                 (call (var "mupl-filter") (second (var "lst")))
                                 (apair (var "x") (call (var "mupl-filter") (second (var "lst"))))))))))

(define mupl-all-gt
  (mlet "filter" mupl-filter
        (fun null "i"
             (call (var "filter") (fun null "x" (isgreater (var "x") (var "i")))))))
             

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))