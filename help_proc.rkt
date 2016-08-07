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

(define member?
  (lambda (a lat)
    (letrec
        [(M? (lambda (lat)
               (cond
                 [(null? lat) #f]
                 [else (or (eqan? (car lat) a)
                           (M? (cdr lat)))])))]
      (M? lat))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (cadr p)))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define firsts
  (lambda (l)
    (if (null? l)
        '()
        (cons (caar l)
              (firsts (cdr l))))))

(define seconds
  (lambda (l)
    (if (null? l)
        '()
        (cons (cadar l)
              (seconds (cdr l))))))

(define pick
  (lambda (n l)
    (if (one? n)
        (car l)
        (pick (sub1 n) (cdr l)))))

(provide atom?)
(provide one?)
(provide eqan?)
(provide eqlist?)
(provide equal?)
(provide member?)
(provide first)
(provide second)
(provide build)
(provide firsts)
(provide seconds)
(provide pick)