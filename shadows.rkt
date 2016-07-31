#lang racket

(require "help_proc.rkt")
(require "arithmetic.rkt")

;;;; 6. Shadows

#|
(define numbered?
  (lambda (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [(eq? (cadr aexp) 'o+)
       (and (numbered? (car aexp))
            (numbered? (caddr aexp)))]
      [(eq? (cadr aexp) 'o*)
       (and (numbered? (car aexp))
            (numbered? (caddr aexp)))]
      [(eq? (cadr aexp) 'o^)
       (and (numbered? (car aexp))
            (numbered? (caddr aexp)))])))
|#

(define numbered?
  (lambda (aexp)
    (cond
      [(atom? aexp) (number? aexp)]
      [else (and (numbered? (car aexp))
                 (numbered? (caddr aexp)))])))

(numbered? 1)

(numbered? '(3 o+ (4 o* 5)))

(numbered? '(3 o+ (4 o^ 5)))

(numbered? '(2 o* sausage))


#|
(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (cadr nexp) 'o+)
       (o+ (value (car nexp))
           (value (caddr nexp)))]
      [(eq? (cadr nexp) 'o*)
       (o* (value (car nexp))
           (value (caddr nexp)))]
      [(eq? (cadr nexp) 'o^)
       (o^ (value (car nexp))
           (value (caddr nexp)))])))

(value 13)

(value '(1 o+ 3))

(value '(1 o+ (3 o^ 4)))

;; (value 'cookie)
|#

#|
(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (car nexp) 'o+)
       (o+ (value (cadr nexp))
           (value (caddr nexp)))]
      [(eq? (car nexp) 'o*)
       (o* (value (cadr nexp))
           (value (caddr nexp)))]
      [(eq? (car nexp) 'o^)
       (o^ (value (cadr nexp))
           (value (caddr nexp)))])))
|#

(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (operator nexp) 'o+)
       (o+ (value (1st-sub-exp nexp))
           (value (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) 'o*)
       (o* (value (1st-sub-exp nexp))
           (value (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) 'o^)
       (o^ (value (1st-sub-exp nexp))
           (value (2nd-sub-exp nexp)))])))

(define 1st-sub-exp
  (lambda (aexp)
    (cadr aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (caddr aexp)))

(define operator
  (lambda (aexp)
    (car aexp)))

(value '(o+ 1 3))



(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define O+
  (lambda (m n)
    (if (sero? n)
        m
        (edd1 (O+ m (zub1 n))))))


(define lat?
  (lambda (l)
    (if (null? l)
        #t
        (and (atom? (car l))
             (lat? (cdr l))))))

(lat? '(1 2 3))

(lat? '((()) (() ()) (() () ())))