#lang racket

(require "help_proc.rkt")

;;;; 4. Numbers Games
;;
;; 此章节对于数的概念，只考虑自然数

(atom? 14)

(number? -3)

(number? 3.14159)

(add1 67)

(sub1 5)

;; (sub1 0)

(zero? 0)

(zero? 1492)


(define o+
  (lambda (m n)
    (if (zero? n)
        m
        (add1 (o+ m (sub1 n))))))

(o+ 46 12)


(define o-
  (lambda (m n)
    (if (zero? n)
        m
        (sub1 (o- m (sub1 n))))))

(o- 14 3)

(o- 17 9)

;; (o- 18 25)


(define tup?
  (lambda (lat)
    (if (null? lat)
        #t
        (and (number? (car lat))
             (tup? (cdr lat))))))

(tup? '(2 11 3 79 47 6))

(tup? '(8 55 5 555))

(tup? '(1 2 8 apple 4 3))

(tup? '(3 (7 4) 13 9))

(tup? '())


(define addtup
  (lambda (tup)
    (if (null? tup)
        0
        (o+ (car tup)
            (addtup (cdr tup))))))

(addtup '(3 5 2 8))

(addtup '(15 6 7 12 3))


(define o*
  (lambda (m n)
    (if (zero? n)
        0
        (o+ m (o* m (sub1 n))))))

(o* 5 3)

(o* 13 4)


#|
(define tup+
  (lambda (tup1 tup2)
    (cond
      [(null? tup1) '()]
      [else (cons (o+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))])))
|#

(define tup+
  (lambda (tup1 tup2)
    (cond
      [(null? tup1) tup2]
      [(null? tup2) tup1]
      [else (cons (o+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))])))

(tup+ '(3 6 9 11 4) '(8 5 2 0 7))

(tup+ '(2 3) '(4 6))

(tup+ '(3 7) '(4 6 8 1))


(define o>
  (lambda (m n)
    (cond
      [(zero? m) #f]
      [(zero? n) #t]
      [else (o> (sub1 m) (sub1 n))])))

(o> 12 133)

(o> 120 11)

(o> 3 3)


(define o<
  (lambda (m n)
    (cond
      [(zero? n) #f]
      [(zero? m) #t]
      [else (o< (sub1 m) (sub1 n))])))

(o< 4 6)

(o< 8 3)

(o< 6 6)


#|
(define o=
  (lambda (m n)
    (cond
      [(zero? m) (zero? n)]
      [(zero? n) #f]
      [else (o= (sub1 m) (sub1 n))])))
|#

(define o=
  (lambda (m n)
    (and (not (o< m n))
         (not (o> m n)))))

(o= 3 3)

(o= 3 1)


(define o^
  (lambda (m n)
    (if (zero? n)
        1
        (o* m (o^ m (sub1 n))))))

(o^ 1 1)

(o^ 2 3)

(o^ 5 3)


(define o/
  (lambda (m n)
    (if (o< m n)
        0
        (add1 (o/ (o- m n) n)))))

(o/ 3 5)

(o/ 5 3)

(o/ 15 4)


(define length
  (lambda (lat)
    (if (null? lat)
        0
        (add1 (length (cdr lat))))))

(length '(hotdogs with mustard sauerkraut and pickles))

(length '(ham and cheese on rye))


(define pick
  (lambda (n lat)
    (if (one? n)
        (car lat)
        (pick (sub1 n) (cdr lat)))))

(pick 4 '(lasagna spaghetti ravioli macaroni meatball))

;; (pick 0 '(a))


(define rempick
  (lambda (n lat)
    (if (one? n)
        (cdr lat)
        (cons (car lat)
              (rempick (sub1 n) (cdr lat))))))

(rempick 3 '(hotdogs with hot mustard))


(number? 'tomato)

(number? 76)


(define no-nums
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(number? (car lat))
       (no-nums (cdr lat))]
      [else (cons (car lat)
                  (no-nums (cdr lat)))])))

(no-nums '(a b c 1 2 3))


(define all-nums
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(number? (car lat))
       (cons (car lat)
             (all-nums (cdr lat)))]
      [else (all-nums (cdr lat))])))

(all-nums '(a 1 b 2 c 3))


(define eqan?
  (lambda (a1 a2)
    (cond
      [(and (number? a1) (number? a2))
       (= a1 a2)]
      [(or (number? a1) (number? a2))
       #f]
      [else (eq? a1 a2)])))

(eqan? 1 1)

(eqan? 1 'a)

(eqan? 'a 'a)


(define occur
  (lambda (a lat)
    (cond
      [(null? lat) 0]
      [(eqan? (car lat) a)
       (add1 (occur a (cdr lat)))]
      [else (occur a (cdr lat))])))

(occur 1 '(a 1 b 1 c 3))