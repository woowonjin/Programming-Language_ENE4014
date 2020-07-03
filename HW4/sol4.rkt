#lang racket

(define (node_number node)
  (car node))
(define (left_node node)
  (car (cdr node)))
(define (right_node node)
  (car (cdr (cdr node))))

(define (check_bst bst)
  (if (null? bst)
      #t
      (letrec ([parent_number (node_number bst)]
            [check_left (lambda(node)
                      (if (null? node)
                          #t
                          (> parent_number (node_number node))))]
            [check_right (lambda(node)
                      (if (null? node)
                          #t
                          (< parent_number (node_number node))))])
        (and (check_left (left_node bst)) (check_right (right_node bst)) (check_bst (left_node bst)) (check_bst (right_node bst))))))

(define (apply function bst)
  (if (null? bst)
      null
      (list (function (node_number bst)) (apply function (left_node bst)) (apply function (right_node bst)))))

(define (exist num bst)
  (if (null? bst)
      #f
      (or (= num (node_number bst)) (exist num (left_node bst)) (exist num (right_node bst)))))

(define (compare bst1 bst2)
  (if (null? bst1)
      #t
      (and (exist (node_number bst1) bst2) (compare (left_node bst1) bst2) (compare (right_node bst1) bst2))))

(define (equals bst1 bst2)
  (and (compare bst1 bst2) (compare bst2 bst1)))
 