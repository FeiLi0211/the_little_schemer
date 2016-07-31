#lang racket

(require "help_proc.rkt")

;;;; 5. *Oh My Gawd*: It's Full of Stars

(define rember*
  (lambda (a l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (if (eqan? (car l) a)
           (rember* a (cdr l))
           (cons (car l)
                 (rember* a (cdr l))))]
      [else (cons (rember* a (car l))
                  (rember* a (cdr l)))])))

(rember* 'cup
         '((coffee) cup ((tea) cup) (and (hick)) cup))

(rember* 'sauce
         '(((tomato sauce))
           ((bean) sauce)
           (and ((flying)) sauce)))


(define lat?
  (lambda (l)
    (if (null? l)
        #t
        (and (atom? (car l))
             (lat? (cdr l))))))

(lat? '(((tomato sauce))
        ((bean) sauce)
        (and ((flying)) sauce)))

(lat?
 (car '(((tomato sauce))
        ((bean) sauce)
        (and ((flying)) sauce))))


(define insertR*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (if (eqan? (car l) old)
           (cons old
                 (cons new
                       (insertR* new old (cdr l))))
           (cons (car l)
                 (insertR* new old (cdr l))))]
      [else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))])))

(insertR* 'roast
          'chuck
          '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))


(define occur*
  (lambda (a l)
    (cond
      [(null? l) 0]
      [(atom? (car l))
       (if (eqan? (car l) a)
           (add1 (occur* a (cdr l)))
           (occur* a (cdr l)))]
      [else (+ (occur* a (car l))
               (occur* a (cdr l)))])))

(occur* 'banana
        '((banana)
          (split ((((banana ice)))
                  (cream (banana))
                  sherbet))
          (banana)
          (bread)
          (banana brandy)))


(define subst*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (if (eqan? (car l) old)
           (cons new (subst* new old (cdr l)))
           (cons (car l) (subst* new old (cdr l))))]
      [else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))])))

(subst* 'orange
        'banana
        '((banana)
          (split ((((banana ice)))
                  (cream (banana))
                  sherbet))
          (banana)
          (bread)
          (banana brandy)))


(define insertL*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (if (eqan? (car l) old)
           (cons new
                 (cons old
                       (insertL* new old (cdr l))))
           (cons (car l)
                 (insertL* new old (cdr l))))]
      [else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))])))

(insertL* 'pecker
          'chuck
          '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))


(define member*
  (lambda (a l)
    (cond
      [(null? l) #f]
      [(atom? (car l))
       (or (eqan? (car l) a)
           (member* a (cdr l)))]
      [else (or (member* a (car l))
                (member* a (cdr l)))])))

(member* 'chips
         '((potato) (chips ((with) fish) (chips))))


(define leftmost
  (lambda (l)
    (if (atom? (car l))
        (car l)
        (leftmost (car l)))))

(leftmost '((potato) (chips ((with) fish) (chips))))

(leftmost '(((hot) (tuna (and))) cheese))

;; (leftmost '(((() four)) 17 (seventeen)))

;; (leftmost '())


#|
(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [(and (atom? (car l1)) (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))]
      [(or (atom? (car l1)) (atom? (car l2))) #f]
      [else (and (eqlist? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))])))
|#

(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2))
       #t]
      [(or (null? l1) (null? l2))
       #f]
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

(eqlist? '(strawberry ice cream)
         '(strawberry ice cream))

(eqlist? '(strawberry ice cream)
         '(strawberry cream ice))

(eqlist? '(banana ((split)))
         '((banana) (split)))

(eqlist? '(beef ((sausage)) (and (soda)))
         '(beef ((sausage)) (and (soda))))

(equal? '((a b) (c d))
        '((a b) (c d)))


(define rember
  (lambda (s l)
    (cond
      [(null? l) '()]
      [(equal? (car l) s) (cdr l)]
      [else (cons (car l)
                  (rember s (cdr l)))])))
      