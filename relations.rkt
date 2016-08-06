#lang racket

(require "help_proc.rkt")

;;;; 7. Friends and Relations

(define set?
  (lambda (lat)
    (cond
      [(null? lat) #t]
      [else
       (and (not (member? (car lat) (cdr lat)))
            (set? (cdr lat)))])))

(set? '(apple peaches apple plum))

(set? '(apples peaches pears plums))

(set? '())

(set? '(apple 3 pear 4 9 apple 3 4))

#|
(define makeset
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [(member? (car lat) (cdr lat))
       (makeset (cdr lat))]
      [else (cons (car lat)
                  (makeset (cdr lat)))])))
|#

(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) '()]
      [(eqan? (car lat) a)
       (multirember a (cdr lat))]
      [else (cons (car lat)
                  (multirember a (cdr lat)))])))

(define makeset
  (lambda (lat)
    (cond
      [(null? lat) '()]
      [else (cons (car lat)
                  (makeset
                   (multirember (car lat) (cdr lat))))])))

(makeset '(apple peach pear peach plum apple lemon peach))

(makeset '(apple 3 pear 4 9 apple 3 4))


(define subset?
  (lambda (set1 set2)
    (cond
      [(null? set1) #t]
      [else
       (and (member? (car set1) set2)
            (subset? (cdr set1) set2))])))

(subset? '(5 chicken wings)
         '(5 hamburgers 2 pieces fried chicken and light ducking wings))

(subset? '(4 pounds of horseradish)
         '(four pounds chicken and 5 ounces horseradish))


(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(eqset? '(6 large chickens with wings)
        '(6 chickens with large wings))


(define intersect?
  (lambda (set1 set2)
    (cond
      [(null? set1) #f]
      [else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))])))

(intersect? '(stewed tomatoes and macaroni)
            '(macaroni and cheese))


(define intersect
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [(member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2))]
      [else (intersect (cdr set1) set2)])))

(intersect '(stewed tomatoes and macaroni casserole)
           '(macaroni and cheese))


(define union
  (lambda (set1 set2)
    (cond
      [(null? set1) set2]
      [(member? (car set1) set2)
       (union (cdr set1) set2)]
      [else (cons (car set1)
                  (union (cdr set1) set2))])))

(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))


(define difference
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [(member? (car set1) set2)
       (difference (cdr set1) set2)]
      [else (cons (car set1)
                  (difference (cdr set1) set2))])))

(difference '(stewed tomatoes and macaroni casserole)
            '(macaroni and cheese))


(define intersectall
  (lambda (l-set)
    (cond
      [(null? (cdr l-set))
       (car l-set)]
      [else (intersect (car l-set)
                       (intersectall (cdr l-set)))])))

(intersectall
 '((a b c) (c a d e) (e f g h a b)))

(intersectall '((6 pears and)
                (3 peaches and 6 peppers)
                (8 pears and 6 plums)
                (and 6 prunes with some apples)))


(define a-pair?
  (lambda (x)
    (cond
      [(atom? x) #f]
      [(null? x) #f]
      [(null? (cdr x)) #f]
      [(null? (cddr x)) #t]
      [else #f])))

(a-pair? '(pear pear))

(a-pair? '(3 7))

(a-pair? '((2) (pair)))

(a-pair? '(full (house)))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (cadr p)))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (l)
    (caddr l)))


(define rel?
  (lambda (l)
    (letrec
        [(S? (lambda (l)
               (if (null? l)
                   #t
                   (and (not (M? (car l) (cdr l)))
                        (S? (cdr l))))))
         (M? (lambda (a l)
               (if (null? l)
                   #f
                   (or (equal? (car l) a)
                       (M? a (cdr l))))))
         (R? (lambda (l)
               (cond
                 [(null? l) #f]
                 [(null? (cdr l)) (a-pair? (car l))]
                 [else (and (a-pair? (car l))
                            (R? (cdr l)))])))]
      (and (S? l) (R? l)))))

(rel? '(apples peaches pumpkin pie))

(rel? '((apples peaches)
        (pumpkin pie)
        (apples peaches)))

(rel? '((apples peaches) (pumpkin pie)))

(rel? '((4 3) (4 2) (7 6) (6 2) (3 4)))



(define fun?
  (lambda (rel)
    (set? (firsts rel))))

#|
(define revrel
  (lambda (rel)
    (cond
      [(null? rel) '()]
      [else (cons (build (second (car rel))
                         (first (car rel)))
                  (revrel (cdr rel)))])))
|#

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (if (null? rel)
        '()
        (cons (revpair (car rel))
              (revrel (cdr rel))))))

(fun? '((4 3) (4 2) (7 6) (6 2) (3 4)))

(fun? '((8 3) (4 2) (7 6) (6 2) (3 4)))

(fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))


#|
(define fullfun?
  (lambda (fun)
    (fun? (revrel fun))))
|#

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))

(fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))

(fullfun? '((grape raisin)
            (plum prune)
            (stewed prune)))

(fullfun? '((grape raisin)
            (plum prune)
            (stewed grape)))