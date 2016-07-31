;;;; 1. Toys

(atom? 'atom)

(atom? 'turkey)

(atom? 1492)

(atom? 'u)

(atom? '*abc$)

(list? '(atom))

(list? '(atom turkey or))

;; (list? (quote (atom turkey) or))

(list? '((atom turkey) or))

(atom? 'xyz)

(list? '(x y z))

(list? '((x y) z))

(list? '(how are you doing so far))

(length '(how are you doing so far))

(list? '(((how) are) ((you) (doing so)) far))

(length '(((how) are) ((you) (doing so)) far))

(list? '())

(atom? '())

(list? '(() () ()))

(car '(a b c))

(car '((a b c) x y z))

;; (car 'hotdog)

;; (car '())

(car '(((hotdogs)) (and) (pickle) relish))

(car '(((hotdogs)) (and) (pickle) relish))

(cdr '(a b c))

(cdr '((a b c) x y z))

(cdr '(hamburger))

(cdr '((x) t r))

;; (cdr 'hotdogs)

;; (cdr '())

(car (cdr '((b) (x y) ((c)))))

(cdr (cdr '((b) (x y) ((c)))))

;; (cdr (car '(a (b (c)) d)))

(cons 'peanut '(butter and jelly))

(cons '(banana and) '(peanut butter and jelly))

(cons '((help) this) '(is very ((hard) to learn)))

(cons '(a b (c)) '())

(cons 'a '())

;; (cons '((a b c)) 'b)

;; (cons 'a 'b)

(cons 'a (car '((b) c d)))

(cons 'a (cdr '((b) c d)))

(null? '())

(null? '(a b c))

;; (null? 'spaghetti)

(atom? 'Harry)

(atom? 'Harry)

(atom? '(Harry had a heap of apples))

(atom? (car '(Harry had a heap of apples)))

(atom? (cdr '(Harry had a heap of apples)))

(atom? (cdr '(Harry)))

(atom? (car (cdr '(swing low sweet cherry oat))))

(atom? (car (cdr '(swing (low sweet) cherry oat))))

(eq? 'Harry 'Harry)

(eq? 'Harry 'Harry)

(eq? 'margarine 'butter)

;; (eq? '() '(strawberry))

;; (eq? 6 7)

(eq? (car '(Marry had a little lamb chop)) 'Mary)

;; (eq? (cdr '(soured milk)) 'milk)

(eq? (car '(beans beans we need jelly beans))
     (car (cdr '(beans beans we need jelly beans))))
