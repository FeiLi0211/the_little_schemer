(define atom?
  (lambda (x)
    (and (not (null? x))
         (not (pair? x)))))

(define one?
  (lambda (n)
    (= n 1)))
