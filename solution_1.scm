;;;;;;;;;;;;;;;;;;; COMP 105 HIGHER FUNC ASSIGNMENT ;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Problem 2


;; (flip f) takes a two-argument function and returns a function that expects
;;          its arguments in the opposite order. Any function that doesn't take
;;          two arguments being applied to flip will be contract violation.

;; laws
;;   ((flip f) xs ys) == (f ys xs)
;;
;; [optional notes about where laws come from, or difficulty, if any]


(define flip (f)
    (lambda (x y)
            (f y x)))

        (check-assert ((flip >) 3 4))
        (check-assert (not ((flip >) 4 3)))
        (check-assert (not ((flip <=) 3 4)))
        (check-expect ((flip append) '(a b c) '(1 2 3)) '(1 2 3 a b c))
        (check-expect ((flip (flip >)) 3 4) (> 3 4))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Problem 3


;; (takewhile p? xs) takes a predicate and a list, returns the longest prefix
;;                  of the list in which every element satisfies the predicate.
;; (dropwhile p? xs) takes a predicate and a list and returns the list that
;;                  removes the longest prefix of the list in which every
;;                  element satisfies the predicate.

;; laws
;;   (takewhile p? '()) == '()
;;   (takewhile p? (cons x as)) = (cons x (takewhile p? as))
;;                                         if (p? x) evaluates to #t
;;                                '()
;;                                         if (p? x) evaluates to #f
;;   (dropwhile p? '()) == '()
;;   (dropwhile p? (cons x as)) == (dropwhile p? as)
;;                                         if (p? x) evaluates to #t
;;                                 (cons x as)
;;                                         if (p? x) evaluates to #f
;; [optional notes about where laws come from, or difficulty, if any]

(define takewhile (p? xs)
    (if (null? xs)
        '()
        (if (p? (car xs))
            (cons (car xs) (takewhile p? (cdr xs)))
            '())))


(define dropwhile (p? xs)
    (if (null? xs)
        '()
        (if (p? (car xs))
            (dropwhile p? (cdr xs))
            xs)))


;; even? returns if x is even, and is defined for unit test purpose
(define even? (x)
    (if (= 0 (mod x 2))
        #t
        #f))

        (check-expect (takewhile atom? '()) '())
        (check-expect (dropwhile atom? '()) '())
        (check-expect (takewhile atom? '(2 4 6 7 8 9)) '(2 4 6 7 8 9))
        (check-expect (dropwhile atom? '(2 4 6 7 8 9)) '())
        (check-expect (takewhile even? '(2 4 6 7 8 10 12)) '(2 4 6))
        (check-expect (dropwhile even? '(2 4 6 7 8 10 12)) '(7 8 10 12))
        (check-expect (takewhile symbol? '(2 3 4 a 8 b c s)) '())
        (check-expect (takewhile symbol? '(a b c s 1 2 3 4)) '(a b c s))
        (check-expect (dropwhile symbol? '(a b c s 1 2 a w 3 4)) '(1 2 a w 3 4))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Problem 4


;; (ordered-by? f) takes one function argument and returns a predicate that
;;                 tells if a list of values is ordered by that relation.
;;

;; laws
;;   ((ordered-by? f) '()) == #t
;;   ((ordered-by? f) (cons x '())) == #t
;;   ((ordered-by? f) (cons x (cons y zs))) == (&& (f x y)
;;                                                ((ordered-by? f) (cons y zs)))
;; [optional notes about where laws come from, or difficulty, if any]

(define ordered-by? (f)
    (lambda (xs)
        (if (null? xs)
            #t
            (if (null? (cdr xs))
                #t
                (and (f (car xs) (cadr xs))
                     ((ordered-by? f) (cdr xs)))))))

        (check-assert (function? ordered-by?))
        (check-assert (function? (ordered-by? <)))
        (check-error (ordered-by? < '(1 2 3)))
        (check-assert ((ordered-by? <) '()))
        (check-assert ((ordered-by? <) '(a)))
        (check-assert ((ordered-by? <) '(1 2 3)))
        (check-assert ((ordered-by? <=) '(1 2 3)))
        (check-assert (not ((ordered-by? <) '(3 2 1))))
        (check-assert ((ordered-by? >=) '(3 2 1)))
        (check-assert ((ordered-by? >=) '(3 3 3)))
        (check-assert ((ordered-by? =) '(3 3 3)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Problem 5

;; (b):
;; (max* xs) takes a non-empty list of integers and returns its maximum element
;; the foldl in max function takes a function and two value arguments, it
;;           compares each element with the next in the list and return the max
;;           element in the list.

(define max* (xs)
    (foldl (lambda (curr_max comp_max)
               (if (< curr_max comp_max)
                   comp_max
                   curr_max))
            (car xs)
            xs))

        (check-expect (max* '(1 2 3 4)) 4)
        (check-expect (max* '(1 2 192 4)) 192)
        (check-expect (max* '(1 290 3 4)) 290)
        (check-expect (max* '(100 2 3 4)) 100)

;; (e):
;; (sum xs) takes a non-empty list of integers and returns the sum of its
;;          elements

(define sum (xs)
    (foldl + 0 xs))

        (check-expect (sum '(1 2 3 4)) 10)
        (check-expect (sum '(10 40 20 30)) 100)
        (check-expect (sum '(109 201 293)) 603)

;; (f):
;; (product xs) takes a non-empty list of integers and returns the products of
;;              its elements
;;

(define product (xs)
    (foldl * 1 xs))

        (check-expect (product '(1 2 3 4)) 24)
        (check-expect (product '(10 40 20 30)) 240000)
        (check-expect (product '(109 201 293)) 6419337)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Problem 6

;; (a):
;; (append xs ys) takes two lists and returns a list of '(xs ys)
;;

(define append(xs ys)
    (foldr cons (foldr cons '() ys) xs))

        (check-expect (append '(a b c d) '(1 2 3 4)) '(a b c d 1 2 3 4))

;; (c):
;; (reverse xs) takes a list and returns a list of its elements in reverse order
;;

(define rev(xs)
    (foldl cons '() xs))

        (check-expect (rev '(a b c d)) '(d c b a))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Problem 7

;; (map f xs) takes a function and a list argument, and returns a list of
;;            results of applying function to each element in the list
;;

(define map (f xs)
    (foldr (lambda (x y)
               (cons (f x) y))
            '()
            xs))

        (check-expect (map (lambda (x) (* x x)) '(1 2 3 4)) '(1 4 9 16))

;; (filter f xs) takes a function and a list argument, and returns a list of
;;               elements satisfying the function
;;

(define filter (f xs)
    (foldr (lambda (x y)
               (if (f x)
                   (cons x y)
                   y))
            '()
            xs))

        (check-expect (filter symbol? '(1 2 a 3 b 8 c o)) '(a b c o))

;; (exists? f xs) takes a function and a list argument, and returns whether
;;                there is an element that satisfies the function
;;

(define exists? (f xs)
    (foldr (lambda (x y)
               (or (f x) y))
            #f
            xs))

        (check-assert (exists? symbol? '(1 2 a 3 b 8 c o)))
        (check-assert (not (exists? number? '(a b c d))))


;; (all? f xs) takes a function and a list argument, and returns whether
;;             every element in the list satisfies the function
;;

(define all? (f xs)
    (foldr (lambda (x y)
               (and (f x) y))
            #t
            xs))

        (check-assert (all? symbol? '(a b c d)))
        (check-assert (not (all? number? '(a b c d))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Problem 8

;; (a):
;; (even? i) takes one argument and returns if it's an even number

(val evens (lambda (i)
                   (= 0 (mod i 2))))

        (check-assert (evens 2))
        (check-assert (not (evens 7)))

;; (b):
;; (two-digits? v) takes one argument and return if it's within [10, 99]

(val two-digits (lambda (i)
                        (and (>= i 10) (<= i 99))))

        (check-assert (two-digits 29))
        (check-assert (not (two-digits 7)))
        (check-assert (not (two-digits 117)))


;; (c):
;; (add-element x s) takes a value and a set argument and returns a new set
;;                   with x being added into it.
;; (lambda v) takes one argument and returns true if this argument is equal
;;            to x or within set s.
;;
;; Laws:
;; (member? x (add-element x s)) == #t
;; (member? x (add-element y s)) == (s x), where (not (equal? y x))

;; member function is defined for unit test purpose, it takes two arguments, a
;;        value and a set and return if the value is in set

(define member? (x s) (s x))


(define add-element (x s)
    (lambda (v)
            (or (equal? x v) (s v))))

        (check-assert (member? 3 (add-element 3 evens)))
        (check-assert (not (member? 3 (add-element 9 evens))))


;; (union s1 s2) takes two set arguments and returns a new set that is the union
;;               of s1 and s2, elements belongs in either s1 or s2.
;; (lambda v) takes one arguments and returns if v is in either set s1 or set s2
;;
;; Laws:
;; (member? x (union s1 s2))     == (or (s1 x) (s2 x))

(define union (s1 s2)
    (lambda (v)
            (or (s1 v) (s2 v))))

        (check-assert (member? 4 (union two-digits evens)))
        (check-assert (member? 17 (union two-digits evens)))
        (check-assert (not (member? 3 (union two-digits evens))))
        (check-assert (not (member? 107 (union two-digits evens))))

;; (inter s1 s2) takes two set arguments and returns a new set that is the
;;               intersection of s1 and s2, elements belongs in both s1 and s2.
;; (lambda v) takes one argument and returns if v is within set s1 and s2
;;
;; Laws:
;; (member? x (inter s1 s2))     == (and (s1 x) (s2 x))

(define inter (s1 s2)
    (lambda (v)
            (and (s1 v) (s2 v))))

        (check-assert (member? 20 (inter two-digits evens)))
        (check-assert (member? 68 (inter two-digits evens)))
        (check-assert (not (member? 4 (inter two-digits evens))))
        (check-assert (not (member? 97 (inter two-digits evens))))
        (check-assert (not (member? 107 (inter two-digits evens))))
        (check-assert (not (member? 7 (inter two-digits evens))))


;; (diff s1 s2) takes two set arguments and returns a new set that is the
;;              difference of s1 and s2, elements belongs in s1 but not s2.
;; (lambda v) takes one argument and returns if v is within set s1 and not
;;            in set s2.
;; Laws:
;; (member? x (diff  s1 s2))     == (and (s1 x) (not (s2 x)))

(define diff (s1 s2)
    (lambda (v)
            (and (s1 v) (not (s2 v)))))

        (check-assert (member? 91 (diff two-digits evens)))
        (check-assert (member? 29 (diff two-digits evens)))
        (check-assert (not (member? 90 (diff two-digits evens))))
        (check-assert (not (member? 104 (diff two-digits evens))))
        (check-assert (not (member? 107 (diff two-digits evens))))
