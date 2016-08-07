#lang racket

(require "help_proc.rkt")

;;;; 9. ... and Again, and Again, and Again, ...

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (if (number? sorn)
        (keep-looking a (pick sorn lat) lat)
        (eq? sorn a))))

(looking 'caviar '(6 2 4 caviar 5 7 3))

(looking 'caviar '(6 2 grits caviar 5 7 3))

; (looking 'caviar '(7 2 4 7 5 6 3))

; (looking 'caviar '(7 1 2 caviar 5 6 3))


(define eternity
  (lambda (x)
    (eternity x)))


(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))


(shift '((a b) c))

(shift '((a b) (c d)))


(define align
  (lambda (pora)
    (cond
      [(atom? pora) pora]
      [(pair? (first pora))
       (align (shift pora))]
      [else
       (build (first pora)
              (align (second pora)))])))


(define length*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else (+ (length* (first pora))
               (length* (second pora)))])))


(define weight*
  (lambda (pora)
    (cond
      [(atom? pora) 1]
      [else (+ (* (weight* (first pora)) 2)
               (weight* (second pora)))])))


(weight* '((a b) c))

(weight* '(a (b c)))


(define revpair
  (lambda (pair)
    (build (second pair)
           (first pair))))

(define shuffle
  (lambda (pora)
    (cond
      [(atom? pora) pora]
      [(pair? (first pora))
       (shuffle (revpair pora))]
      [else
       (build (first pora)
              (shuffle (second pora)))])))

(shuffle '(a (b c)))

(shuffle '(a b))

; (shuffle '((a b) (c d)))


(define C
  (lambda (n)
    (cond
      [(one? n) 1]
      [else
       (cond
         [(even? n) (C (/ n 2))]
         [else (C (add1 (* n 3)))])])))


(define A
  (lambda (n m)
    (cond
      [(zero? n) (add1 m)]
      [(zero? m) (A (sub1 n) 1)]
      [else (A (sub1 n)
               (A n (sub1 m)))])))

(A 1 0)

(A 1 1)

(A 2 2)


#|
;; 停机问题

(define will-stop?
  (lambda (f)
    ...))

(define last-try
  (lambda (x)
    (and (will-stop? last-try)
         (eterntity x))))

;; 如果last-try会停机
;; (will-stop? last-try) 为 #t
;; 根据and运算符运算规则，此时计算 (eternity x)
;; 结果无法停机
;; 引出矛盾

;; 如果last-try不会停机
;; (will-stop? last-try) 为 #f
;; 根据and运算符运算规则，此时 last-try 结算结束
;; 引出矛盾

;; 所以， will-stop? 函数不存在
|#


;; What are recursive definitions?

(define length
  (lambda (l)
    (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))])))

(lambda (l)
  (cond
    [(null? l) 0]
    [else (add1 (eternity (cdr l)))]))

(lambda (l)
  (cond
    [(null? l) 0]
    [else (add1 ((lambda (l)
                   (cond
                     [(null? l) 0]
                     [else (add1 (eternity (cdr l)))]))
                 (cdr l)))]))

(lambda (l)
  (cond
    [(null? l) 0]
    [else
     (add1 ((lambda (l)
              (cond
                [(null? l) 0]
                [else
                 (add1 ((lambda (l)
                          (cond
                            [(null? l) 0]
                            [else
                             (add1 (eternity (cdr l)))]))
                        (cdr l)))]))
            (cdr l)))]))


;; 抽象

((lambda (length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))])))
 eternity)

((lambda (length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))])))
 ((lambda (length)
    (lambda (l)
      (cond
        [(null? l) 0]
        [else (add1 (length (cdr l)))])))
  eternity))

((lambda (length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))])))
 ((lambda (length)
    (lambda (l)
      (cond
        [(null? l) 0]
        [else (add1 (length (cdr l)))])))
  ((lambda (length)
     (lambda (l)
       (cond
         [(null? l) 0]
         [else (add1 (length (cdr l)))])))
   eternity)))


;; 抽象

((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))]))))

((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))]))))

((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))]))))

;; 替换 eternity mk-length

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))]))))

;; 替换 length mk-length

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (mk-length (cdr l)))]))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 ((mk-length eternity) (cdr l)))]))))

;; length, 表达式表现了无限的概念
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 ((mk-length mk-length) (cdr l)))]))))
   

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 ((lambda (x)
                      ((mk-length mk-length) x))
                    (cdr l)))]))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          [(null? l) 0]
          [else (add1 (length (cdr l)))])))
    (lambda (x)
      ((mk-length mk-length) x)))))

((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
       [(null? l) 0]
       [else (add1 (length (cdr l)))]))))

(lambda (le)
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
           ((mk-length mk-length) x))))))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

