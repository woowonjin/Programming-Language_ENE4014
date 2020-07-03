#lang racket
(provide (all-defined-out)) ;; exports the defined variables in this file.

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

;; Problem 1

(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))


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
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        
        [(int? e) e]

        [(ifgreater? e)
         (letrec ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if(> (int-num v1) (int-num v2))
                  (eval-under-env (ifgreater-e3 e) env)
                  (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater's argument is not integer")))]
        
        [(apair? e)
         (letrec ([v1 (eval-under-env (apair-e1 e) env)]
                  [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        
        [(fst? e)
         (letrec ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "Not apair")))]
        
        [(snd? e)
         (letrec ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "Not apair")))]

        [(aunit? e) e]

        [(isaunit? e)
         (letrec ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
           (int 1)
           (int 0)))]

        [(closure? e) e]

        ;;fun, call, mlet do..

        [(fun? e) closure env e]

        [(call? e)
         (let ([exp (eval-under-env (call-funexp e) env)]
               [act (eval-under-env (call-actual e) env)])
           (if (closure? exp)
               (let* ([cfun (closure-fun exp)]
                      [cenv (closure-env exp)]
                      [name (cons(fun-nameopt cfun) exp)]
                      [formal (cons(fun-formal cfun) act)])
                 (let ([cfunbody (fun-body cfun)])
                   (eval-under-env cfunbody (if (eq? (car name) #f)
                                                (cons formal cenv)
                                                (cons formal (cons name cenv))))))
               (error "It is not closure")))]

        [(mlet? e)
         (letrec ([v (eval-under-env (mlet-e e) env)]
                  [newenv (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) newenv))]
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_y") e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "func"
       (fun "op" "lst"
            (ifaunit (var "lst")
                     (aunit)
                     (apair (call (var "func") (fst (var "lst"))) (call (var "op") (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "#f" "n"
             (call (var "map") (fun #f "x" (add (var "x") (var"n")))))))


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
