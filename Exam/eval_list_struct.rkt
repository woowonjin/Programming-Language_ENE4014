#lang racket

(provide (all-defined-out))

; helper functions that make lists where first element is a symbol
(define (Const i) (list 'Const i)) ; => ('Const i)
(define (Negate e) (list 'Negate e))
(define (Add e1 e2) (list 'Add e1 e2)) ; => (Add (Const 10) 42)
(define (Multiply e1 e2) (list 'Multiply e1 e2))

(Multiply (Add (Const 10) (Negate (Const 42))) (Const 10))

; helper functions that test what "kind of exp"
(define (Const? x) (eq? (car x) 'Const))
(define (Negate? x) (eq? (car x) 'Negate))
(define (Add? x) (eq? (car x) 'Add))
(define (Multiply? x) (eq? (car x) 'Multiply))

; helper functions that get the pieces for "one kind of exp"
; ('Const 42)
(define (Const-int e) (car (cdr e)))
(define (Negate-e e) (car (cdr e)))
(define (Add-e1 e) (car (cdr e)))
(define (Add-e2 e) (car (cdr (cdr e))))
(define (Multiply-e1 e) (car (cdr e)))
(define (Multiply-e2 e) (car (cdr (cdr e))))

(Negate (Negate (Const 42)))

;(list 'Negate (list 'Negate (list 'Const 42)))

; same recursive structure as we have in ML
; one change: returning an exp rather than an int


;(Negate (Add (Const 10) (Const 11)))

(define (eval-exp1 e)
  (cond [(Const? e) e] ; note returning an exp, not a number
        [(Negate? e) (Const (- (Const-int (eval-exp1 (Negate-e e)))))]
        [(Add? e) (let ([v1 (Const-int (eval-exp1 (Add-e1 e)))]
                        [v2 (Const-int (eval-exp1 (Add-e2 e)))])
                    (Const (+ v1 v2)))]  ; ('Const v1 v2)
        [(Multiply? e) (let ([v1 (Const-int (eval-exp1 (Multiply-e1 e)))]
                             [v2 (Const-int (eval-exp1 (Multiply-e2 e)))])
                         (Const (* v1 v2)))]
        [#t (error "eval-exp expected an exp")]))





(define a-test1 (eval-exp1 (Multiply (Negate (Add (Const 2) (Const 2))) (Const 7))))
;;a-test1 ==> ('Const -28)

; same idea using Racket's structs, which are more convenient /and/ less
; error-prone

(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)



(define (eval-exp2 e)
  (cond [(const? e) e] ; note returning an exp, not a number
        [(negate? e) (const (- (const-int (eval-exp2 (negate-e e)))))]
        [(add? e) (let ([v1 (const-int (eval-exp2 (add-e1 e)))]
                        [v2 (const-int (eval-exp2 (add-e2 e)))])
                    (const (+ v1 v2)))]
        [(multiply? e) (let ([v1 (const-int (eval-exp2 (multiply-e1 e)))]
                             [v2 (const-int (eval-exp2 (multiply-e2 e)))])
                         (const (* v1 v2)))]
        [#t (error "eval-exp expected an exp")]))

;(define a-test2 (eval-exp2 (multiply (negate (add (const 2) (const 2))) (const 7))))





