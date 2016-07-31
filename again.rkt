#lang racket

(require "help_proc.rkt")

;;;; 2. Do It, Do It Again, and Again, and Again ...



(define lat?
  (lambda (l)
    (if (null? l)
        #t
        (and (atom? (car l))
             (lat? (cdr l))))))

(lat? '(Jack Sprat could eat no chicken fat))

(lat? '((Jack) Sprat could eat no chicken fat))

(lat? '())

(lat? '(bacon and eggs))

(lat? '(bacon (and eggs)))

(or (null? '()) (atom? '(d e f g)))

(or (null? '(a b c)) (null? '()))

(or (null? '(a b c)) (null? '(atom)))

(define member?
  (lambda (a lat)
    (if (null? lat)
        #f
        (or (eq? (car lat) a)
            (member? a (cdr lat))))))

(member? 'tea '(coffee tea or milk))

(member? 'poached '(fried eggs and scrambled eggs))

(member? 'meat '(mashed potatoes and meat gravy))

(member? 'liver '(bagels and lox))
