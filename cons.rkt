#lang racket

;;;; 3. Cons the Magnificent

(define rember
  (lambda (a lat)
    (cond
     [(null? lat) '()]
     [(eq? (car lat) a) (cdr lat)]
     [else (cons (car lat) (rember a (cdr lat)))])))

(rember 'mint
        '(lamb chops and mint jelly))

(rember 'mint
        '(lamb chops and mint flavored mint jelly))

(rember 'toast
        '(bacon lettuce and tomato))

(rember 'cup
        '(coffee cup tea cup and hick cup))


(define firsts
  (lambda (l)
    (if (null? l)
        '()
        (cons (caar l)
              (firsts (cdr l))))))

(firsts '((apple peach pumpkin)
          (plum pear cherry)
          (grape raisin pea)
          (bean carrot eggplant)))

(firsts '((a b) (c d) (e f)))

(firsts '())

(firsts '((five plums)
          (four)
          (eleven green oranges)))

(firsts '(((five plums) four)
          (eleven green oranges)
          ((no) more)))


(define seconds
  (lambda (l)
    (if (null? l)
        '()
        (cons (cadar l)
              (seconds (cdr l))))))

(seconds '((a b) (c d) (e f)))


(define insertR
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons old (cons new (cdr lat)))]
      [else (cons (car lat)
                  (insertR new old (cdr lat)))])))

(insertR 'topping
         'fudge
         '(ice cream with fudge for dessert))

(insertR 'jalapeno
         'and
         '(tacos tamales and salsa))

(insertR 'e 'd '(a b c d f g d h))


(define insertL
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons new lat)]
      [else (cons (car lat)
                  (insertL new old (cdr lat)))])))

(insertL 'e 'd '(a b c d f g d h))


(define subst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons new (cdr lat))]
      [else (cons (car lat)
                  (subst new old (cdr lat)))])))

(subst 'topping
       'fudge
       '(ice cream with fudge for dessert))


(define subst2
  (lambda (new o1 o2 lat)
    (cond
      [(null? lat) '()]
      [(or (eq? (car lat) o1)
           (eq? (car lat) o2))
       (cons new (cdr lat))]
      [else (cons (car lat)
                  (subst2 new o1 o2 (cdr lat)))])))

(subst2 'vanilla
        'chocolate
        'banana
        '(banana ice cream with chocolate topping))

(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) a)
       (multirember a (cdr lat))]
      [else (cons (car lat)
                  (multirember a (cdr lat)))])))

(multirember 'cup
             '(coffee cup tea cup and hick cup))


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

(multiinsertR 'e 'd '(a b c d f g d h))


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

(multiinsertL 'e 'd '(a b c d f g d h))

(multiinsertL 'fried
              'fish
              '(chips and fish or fish and fried))


(define multisubst
  (lambda (new old lat)
    (cond
      [(null? lat) '()]
      [(eq? (car lat) old)
       (cons new (multisubst new old (cdr lat)))]
      [else (cons (car lat)
                  (multisubst new old (cdr lat)))])))

(multisubst 'fried
            'fish
            '(chips and fish or fish and fried))