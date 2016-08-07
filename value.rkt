#lang racket

;;;; 10. What Is the Value of All of This?

;; help procedure
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define first car)
(define second cadr)
(define third caddr)

(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

;; entry # a pair of lists

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
     name
     (first entry)
     (second entry)
     entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      [(null? names) (entry-f name)]
      [(eq? (car names) name) (car values)]
      [else
       (lookup-in-entry-help name
                             (cdr names)
                             (cdr values)
                             entry-f)])))

(lookup-in-entry 'entree
                 '((appetizer entree veverage)
                   (food tastes good))
                 (lambda (x)
                   (error 'lookup-in-entry "not found" x)))

;; table/environment # a list of entires

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      [(null? table) (table-f name)]
      [else
       (lookup-in-entry name
                        (car table)
                        (lambda (name)
                          (lookup-in-table name
                                           (cdr table)
                                           table-f)))])))

(lookup-in-table 'entree
                 '(((entree dessert)
                    (spaghetti spumoni))
                   ((appetizer entree veverage)
                    (food tasts good)))
                 (lambda (x)
                   (error 'lookup-in-table
                          "not found"
                          x)))

(car (quote (a b c)))

(cons 'a (cons 'b (cons 'c '())))

(cons 'car
      (cons (cons 'quote
                  (cons
                   (cons 'a
                         (cons 'b
                               (cons 'c
                                     '())))
                   '()))
            '()))

(car '(a b c))

;; -----------------------------------------------------------------------------

;; *const *quote *identifier *lambda *application *cond

(define expression-to-action
  (lambda (e)
    (cond
      [(atom? e) (atom-to-action e)]
      [else (list-to-action e)])))

(define atom-to-action
  (lambda (e)
    (cond
      [(number? e) *const]
      [(eq? e #t) *const]
      [(eq? e #f) *const]
      [(eq? e 'cons) *const]
      [(eq? e 'car) *const]
      [(eq? e 'cdr) *const]
      [(eq? e 'null?) *const]
      [(eq? e 'eq?) *const]
      [(eq? e 'atom?) *const]
      [(eq? e 'zero?) *const]
      [(eq? e 'add1) *const]
      [(eq? e 'sub1) *const]
      [(eq? e 'number?) *const]
      [else *identifier])))

(define list-to-action
  (lambda (e)
    (cond
      [(atom? (car e))
       (cond
         [(eq? (car e) 'quote) *quote]
         [(eq? (car e) 'lambda) *lambda]
         [(eq? (car e) 'cond) *cond]
         [else *application])]
      [else *application])))

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      [(number? e) e]
      [(eq? e #t) #t]
      [(eq? e #f) #f]
      [else (build 'primitive e)])))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evcon
  (lambda (lines table)
    (cond
      [(else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table)]
      [(meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table)]
      [else (evcon (cdr lines) table)])))

(define else?
  (lambda (x)
    (cond
      [(atom? x) (eq? x 'else)]
      [else #f])))

(define question-of first)
(define answer-of second)

(*cond '(cond (coffee klatsch) (else party))
         '(((coffee) (#t))
           ((klatsch party) (5 (6)))))

(define *application
  (lambda (e table)
    (apply (meaning (function-of e) table)
           (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define evlis
  (lambda (args table)
    (cond
      [(null? args) '()]
      [else (cons (meaning (car args) table)
                  (evlis (cdr args) table))])))

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

(define apply
  (lambda (fun vals)
    (cond
      [(primitive? fun)
       (apply-primitive
        (second fun) vals)]
      [(non-primitive? fun)
       (apply-closure
        (second fun) vals)])))

(define apply-primitive
  (lambda (name vals)
    (cond
      [(eq? name 'cons)
       (cons (first vals) (second vals))] ; what's this? that is wrong!
      [(eq? name 'car)
       (car (first vals))]
      [(eq? name 'cdr)
       (cdr (first vals))]
      [(eq? name 'null?)
       (null? (first vals))]
      [(eq? name 'eq?)
       (eq? (first vals) (second vals))]
      [(eq? name 'atom?)
       (:atom? (first vals))]
      [(eq? name 'zero?)
       (zero? (first vals))]
      [(eq? name 'add1)
       (add1 (first vals))]
      [(eq? name 'sub1)
       (sub1 (first vals))]
      [(eq? name 'number?)
       (number? (first vals))])))

(define :atom?
  (lambda (x)
    (cond
      [(atom? x) #t]
      [(null? x) #f]
      [(eq? (car x) 'primitive) #t]
      [(eq? (car x) 'non-primitive) #t]
      [else #f])))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry (formals-of closure)
                         vals)
              (table-of closure)))))

(apply-closure '((((u v w)
                   (1 2 3))
                  ((x y z)
                   (4 5 6)))
                 (x y)
                 (cons z x))
               '((a b c) (d e f)))