#lang racket

(require "help_proc.rkt")
(require "arithmetic.rkt")

;;;; 8. Lambda the Ultimate

(define rember
  (lambda (a l)
    (cond
      [(null? l) '()]
      [(equal? (car l) a) (cdr l)]
      [else (cons (car l)
                  (rember a (cdr l)))])))


#|
(define rember-f
  (lambda (test? a l)
    (cond
      [(null? l) '()]
      [(test? (car l) a) (cdr l)]
      [else (cons (car l)
                  (rember-f test? a (cdr l)))])))
|#

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        [(null? l) '()]
        [(test? (car l) a) (cdr l)]
        [else (cons (car l)
                    ((rember-f test?) a (cdr l)))]))))

((rember-f =) 5 '(6 2 5 3))

((rember-f equal?) '(pop corn) '(lemonade (pop corn) and (cake)))


(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        [(null? l) '()]
        [(test? (car l) old)
         (cons new (cons old (cdr l)))]
        [else (cons (car l)
                    ((insertL-f test?) new old (cdr l)))]))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        [(null? l) '()]
        [(test? (car l) old)
         (cons old (cons new (cdr l)))]
        [else (cons (car l)
                    ((insertR-f test?) new old (cdr l)))]))))


(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        [(null? l) '()]
        [(eq? (car l) old)
         (seq new old (cdr l))]
        [else (cons (car l)
                    ((insert-g seq) new old (cdr l)))]))))

(define insertL
  (insert-g (lambda (new old l)
              (cons new (cons old l)))))

(define insertR
  (insert-g (lambda (new old l)
              (cons old (cons new l)))))

(define subst
  (insert-g (lambda (new old l)
              (cons new l))))

(define remberv
  (lambda (a l)
    ((insert-g (lambda (new old l)
                 l))
     #f a l)))


(define operator
  (lambda (aexp)
    (car aexp)))

(define 1st-sub-exp
  (lambda (aexp)
    (cadr aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (caddr aexp)))

(define atom-to-function
  (lambda (x)
    (cond
      [(eq? x 'o+) o+]
      [(eq? x 'o*) o*]
      [else o^])))

#|
(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [(eq? (operator nexp) 'o+)
       (o+ (value (1st-sub-exp nexp))
           (value (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) 'o+)
       (o+ (value (1st-sub-exp nexp))
           (value (2nd-sub-exp nexp)))]
      [(eq? (operator nexp) 'o+)
       (o+ (value (1st-sub-exp nexp))
           (value (2nd-sub-exp nexp)))])))
|#

(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [else ((atom-to-function
              (operator nexp))
             (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp)))])))

(value '(o+ 1 4))


(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) a)
       (multirember a (cdr lat))]
      [else (cons (car lat)
                  (multirember a (cdr lat)))])))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) '()]
        [(test? (car lat) a)
         ((multirember-f test?) a (cdr lat))]
        [else (cons (car lat)
                    ((multirember-f test?) a (cdr lat)))]))))

(define multiremberT
  (lambda (test? lat)
    (cond
      [(null? lat) '()]
      [(test? (car lat))
       (multiremberT test? (cdr lat))]
      [else (cons (car lat)
                  (multiremberT test? (cdr lat)))])))

(multiremberT (lambda (x)
                (eq? x 'tuna))
              '(shrimp salad tuna salad and tuna))


;; 可以把Collector理解为一个栈结构吗？
;; col的每个变量都是一个栈，符合条件的数据入对应的栈
;; 在结束递归的时候，执行相应的操作
(define multirember&co
  (lambda (a lat col)
    (cond
      [(null? lat)
       (col '() '())]
      [(eq? (car lat) a)
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col newlat
                              (cons (car lat) seen))))]
      [else
       (multirember&co a
                       (cdr lat)
                       (lambda (newlat seen)
                         (col (cons (car lat) newlat)
                              seen)))])))

(multirember&co 'tuna
                '(strawberries tuna and swordfish)
                (lambda (x y)
                  (list x y)))


(define multiinsertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons new
             (cons old
                   (multiinsertL new old (cdr lat))))]
      [else (cons (car lat)
                  (multiinsertL new old (cdr lat)))])))

(define multiinsertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons old
             (cons new
                   (multiinsertR new old (cdr lat))))]
      [else (cons (car lat)
                  (multiinsertR new old (cdr lat)))])))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR (cdr lat))))]
      [(eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR (cdr lat))))]
      [else (cons (car lat)
                  (multiinsertLR new oldL oldR (cdr lat)))])))

(multiinsertLR 'c 'd 'b '(a b c d a b c d))


(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      [(null? lat)
       (col '() 0 0)]
      [(eq? (car lat) oldL)
       (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons new (cons oldL newlat))
                                (add1 L)
                                R)))]
      [(eq? (car lat) oldR)
       (multiinsertLR&co new
                         oldL
                         oldR
                         (cdr lat)
                         (lambda (newlat L R)
                           (col (cons oldR (cons new newlat))
                                L
                                (add1 R))))]
      [else
       (multiinsertLR&co new
                          oldL
                          oldR
                          (cdr lat)
                          (lambda (newlat L R)
                            (col (cons (car lat) newlat)
                                 L
                                 R)))])))

(multiinsertLR&co 'c 'd 'b '(a b d a b d) (lambda (newlat L R)
                                            (list newlat L R)))


(define evens-only*
  (lambda (l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(even? (car l))
          (cons (car l)
                (evens-only* (cdr l)))]
         [else (evens-only* (cdr l))])]
      [else (cons (evens-only* (car l))
                  (evens-only* (cdr l)))])))

(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))


(define evens-only*&co
  (lambda (l col)
    (cond
      [(null? l)
       (col '() 1 0)]
      [(atom? (car l))
       (cond
         [(even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl)
                                 (* p (car l))
                                 s)))]
         [else
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col newl
                                 p
                                 (+ s (car l)))))])]
      [else
       (evens-only*&co (car l)
                       (lambda (al ap as)
                         (evens-only*&co (cdr l)
                                         (lambda (dl dp ds)
                                           (col (cons al dl)
                                                (* ap dp)
                                                (+ as ds))))))])))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
                (lambda (newl p s)
                  (list newl p s)))
         