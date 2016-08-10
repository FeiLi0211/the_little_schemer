# the_little_schemer
阅读『The Little Schemer』之后，代码留档

* Y combinator

```  
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
```

* Collector (Stack)

```
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
                                 (* (car l) p)
                                 s)))]
         [else
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col newl
                                 p
                                 (+ (car l) s))))])]
      [else
       (evens-only*&co (car l)
                       (lambda (al ap as)
                         (evens-only*&co (cdr l)
                                         (lambda (dl dp ds)
                                           (col (cons al dl)
                                                (* ap dp)
                                                (+ as ds))))))])))
```


* Recursive

```
(define length
  (lambda (l)
    (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))])))
```
* Value 这个基于类型的求值器不太实用啊！！！

```
(define environment '())

(define value
  (lambda (e)
    (meaning e environment)))
```


> 上善如水 道法自然 知行合一

> Thanks for "Daniel P. Friedman" & "Matthias Felleisen"