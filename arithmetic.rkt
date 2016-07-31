#lang racket

(define o+
  (lambda (m n)
    (if (zero? n)
        m
        (add1 (o+ m (sub1 n))))))

(define o-
  (lambda (m n)
    (if (zero? n)
        m
        (sub1 (o- m (sub1 n))))))

(define o*
  (lambda (m n)
    (if (zero? n)
        0
        (o+ m (o* m (sub1 n))))))

(define o^
  (lambda (m n)
    (if (zero? n)
        1
        (o* m (o^ m (sub1 n))))))

(define o/
  (lambda (m n)
    (if (< m n)
        0
        (add1 (o/ (o- m n) n)))))

(provide o+)
(provide o-)
(provide o*)
(provide o/)
(provide o^)