;; Programming Languages, Homework 5 version 1.1
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem A

; todo unsure how to represent an empty list in mupl
(define (mupllist->racketlist lst)
  (if (aunit? lst)
      '() 
      (let* ([tail (apair-e2 lst)]
             [head (apair-e1 lst)]
             [headprime (if (apair? head) (mupllist->racketlist head) head)])
        (if (aunit? tail) (list headprime)  
            (cons headprime (mupllist->racketlist tail))))))

(define (racketlist->mupllist lst) 
  (if (null? lst) (aunit) ; unsure how to do empty list here
    (apair (car lst) (racketlist->mupllist (cdr lst)))))

;; Problem B

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
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]        

        [(ifgreater? e) 
         (let ([g (eval-under-env (ifgreater-e1 e) env)]
               [l (eval-under-env (ifgreater-e2 e) env)])
           (if (not (and (int? g) (int? l))) 
             (error "MUPL comparison applied to non-integer")
             (if (> (int-num g) (int-num l)) 
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env))))]

         [(fun? e) (closure env e)]

         [(call? e)
          (let (
                [clsr (eval-under-env (call-funexp e) env)]
                [actual (eval-under-env (call-actual e) env)])
            (if (closure? clsr)
                (let* ([f (closure-fun clsr)]
                       [f-env (closure-env clsr)]
                       [f-name (fun-nameopt f)]
                       [f-formal (fun-formal f)]
                       [f-body (fun-body f)]
                       ; add function name to env
                       ; add param to env
                       [f-name-binding (if f-name (cons f-name clsr) #f)]
                       [f-env (if f-name-binding (cons f-name-binding f-env) f-env)]
                       [param-binding (cons f-formal actual)]
                       [f-env (cons param-binding f-env)])
                  (eval-under-env f-body f-env))                 
                (error "call requires a MUPL function")))]
             
         [(mlet? e) 
          (let ([var (mlet-var e)] 
                [exp (mlet-e e)] 
                [body (mlet-body e)])
            (if (not (string? var)) 
              (error "MUPL mlet requires a string variable name")
              (let* ([binding (cons var (eval-under-env exp env))]
                     [newenv (cons binding env)])
                (eval-under-env body newenv))))]
         [(apair? e) 
          (apair 
            (eval-under-env (apair-e1 e) env) 
            (eval-under-env (apair-e2 e) env))]

         [(fst? e) 
          (define p (eval-under-env (fst-e e) env))
          (if (apair? p) 
            (eval-under-env (apair-e1 p) env)
            (error "fst requires a MUPL pair"))]

         [(snd? e) 
          (define p (eval-under-env (snd-e e) env))
          (if (apair? p) 
            (eval-under-env (apair-e2 p) env)
            (error "snd requires a MUPL pair"))]

        [(aunit? e) e]

        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]

        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem C

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* pairs e)
  (if (null? pairs)
      e
      (mlet (caar pairs) (cdar pairs) (mlet* (cdr pairs) e))))

(define (ifeq e1 e2 e3 e4) ; e3 if e1 == e2 (both ints), else e4
  (ifgreater e1 e2 e4 (ifgreater e2 e1 e4 e3)))

;; Problem D

(define mupl-map "CHANGE")
;; this binding is a bit tricky. it must return a function.
;; the first two lines should be something like this:
;;
;;   (fun "mupl-map" "f"    ;; it is  function "mupl-map" that takes a function f
;;       (fun #f "lst"      ;; and it returns an anonymous function
;;          ...
;;
;; also remember that we can only call functions with one parameter, but
;; because they are curried instead of
;;    (call funexp1 funexp2 exp3)
;; we do
;;    (call (call funexp1 funexp2) exp3)
;; 

(define mupl-mapAddN
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))