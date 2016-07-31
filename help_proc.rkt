#lang racket

(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

(define one?
  (lambda (n)
    (= n 1)))

(define eqan?
  (lambda (a1 a2)
    (cond
      [(and (number? a1) (number? a2))
       (= a1 a2)]
      [(or (number? a1) (number? a2))
       #f]
      [else (eq? a1 a2)])))

(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [else (and (equal? (car l1) (car l2))
                 (equal? (cdr l1) (cdr l2)))])))

(define equal?
  (lambda (s1 s2)
    (cond
      [(and (atom? s1) (atom? s2))
       (eqan? s1 s2)]
      [(or (atom? s1) (atom? s2))
       #f]
      [else (eqlist? s1 s2)])))

(provide atom?)
(provide one?)
(provide eqan?)