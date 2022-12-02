;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT ;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 2


;; (contig-sublist? xs ys) returns whether xs is a continuous subsequence of ys
;; (compare-sublist? xs ys) returns true if xs is a continuous subsequence of
;;                          ys starting from the first element in ys,
;;                          false otherwise

;; laws
;;   (contig-sublist? '() ys) == #t
;;   (contig-sublist? (cons x 'xs) ys) == (= x (car ys))
;;                                        #f (if ys is an empty list)
;;                                        (compare-sublist? 'xs (cdr ys))
;;                                              (if first element is found)
;;                                        (contig-sublist? xs (cdr ys))
;;                                              (if first element is not found)
;;   (compare-sublist? (cons x as) ys) == #t (if (cons x as) is an empty list)
;;                                    (or (= x  (car ys))
;;                                        (compare-sublist? xs (cdr ys)))
;;                                        (if (cons x as) isn't empty list)
;; [optional notes about where laws come from, or difficulty, if any]


(define compare-sublist? (xs ys)
    (if (null? xs)
        #t
        (if (= (car xs) (car ys))
            (compare-sublist? (cdr xs) (cdr ys))
            #f)))

(define contig-sublist? (xs ys)
    (if (null? xs)
        #t
        (if (null? ys)
            #f
            (if (= (car xs) (car ys))
                (compare-sublist? (cdr xs) (cdr ys))
                (contig-sublist? xs (cdr ys))))))

        (check-assert (contig-sublist? '(95 96 97) '(95 96 97 98 99)))
        (check-assert (contig-sublist? '(96 97 98) '(95 96 97 98 99)))
        (check-assert (contig-sublist? '(97 98 99) '(95 96 97 98 99)))
        (check-assert (not (contig-sublist? '(96 98) '(95 96 97 98 99))))
        (check-assert (not (contig-sublist? '(95 97 98) '(95 96 97 98 99))))
        (check-assert (not (contig-sublist? '(3 7 6) '(95 96 97 98 99))))
        (check-assert (not (contig-sublist? '(a b c) '(x a y b z c))))
        (check-assert (contig-sublist? '(x) '(x a y b z c)))
        (check-assert (contig-sublist? '() '(x a y b z c)))
        (check-assert (compare-sublist? '() '(a y b z c)))
        (check-assert (compare-sublist? '(a y b) '(a y b z c)))
        (check-assert (not (compare-sublist? '(a b) '(a y b z c))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 3


;; (flatten xs) returns xs without any internal brackets

;; laws:
;;   (flatten '()) == '()
;;   (flatten (cons x xs)) = (cons '(x) (flatten xs))
;;                                      (if x is atom but not empty list)
;;                           (append (flatten x) (flatten xs)) (if x isn't atom)
;;   (flatten (cons '() xs)) = (flatten xs)
;; [optional notes about where laws come from, or difficulty, if any]

(define flatten (xs)
    (if (null? xs)
        '()
        (if (atom? (car xs))
            (if (null? (cdr xs))
                xs
                (if (null? (car xs))
                    (flatten (cdr xs))
                    (append (cons (car xs) '()) (flatten (cdr xs)))))
            (if (null? (cdr xs))
                (flatten (car xs))
                (append (flatten (car xs)) (flatten (cdr xs)))))))

        (check-expect (flatten '((I Ching) (U Thant) (E))) '(I Ching U Thant E))
        (check-expect (flatten '((((((a))))))) '(a))
        (check-expect (flatten '((((((a))))) ((((b)))))) '(a b))
        (check-expect (flatten '()) '())
        (check-expect (flatten '((a) () (b c) d e)) '(a b c d e))
        (check-expect (flatten '(a b c d e)) '(a b c d e))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 4


;; (take n xs) return a list of first n elements in list xs, or xs if n is
;;             larger than the length of xs

;; laws:
;;   (take 0 xs) == '()
;;   (take n '()) == '()
;;   (take n (cons x as)) == xs (when length of xs < n)
;;   (take n (cons x as)) == (append x (take n-1 as)) (when n < length of xs)
;; [optional notes about where laws come from, or difficulty, if any]

(define take (n xs)
    (if (= n 0)
        '()
        (if (null? xs)
            '()
            (append (cons (car xs) '()) (take (- n 1) (cdr xs))))))

        (check-expect (take 0 '(1 2 3 4 99)) '())
        (check-expect (take 2 '()) '())
        (check-expect (take 2 '(1 2 3 4 99)) '(1 2))
        (check-expect (take 5 '(1 2 3 4 99)) '(1 2 3 4 99))
        (check-expect (take 9 '(1 2 3 4 99)) '(1 2 3 4 99))



;; (drop n xs) return a list without first n elements in list xs, or empty
;;             list if n is larger than the length of xs

;; laws:
;;   (drop 0 xs) == xs
;;   (drop n '()) == '()
;;   (drop n xs) == '() (when length of xs < n)
;;   (drop n xs) == (drop n-1 (cdr xs)) (if n < length of xs)
;; [optional notes about where laws come from, or difficulty, if any]

(define drop (n xs)
    (if (= n 0)
        xs
        (if (= (take n xs) xs)
            '()
            (drop (- n 1) (cdr xs)))))

        (check-expect (drop 0 '(1 2 3 4 99)) '(1 2 3 4 99))
        (check-expect (drop 1 '(1 2 3 4 99)) '(2 3 4 99))
        (check-expect (drop 3 '(1 2 3 4 99)) '(4 99))
        (check-expect (drop 5 '(1 2 3 4 99)) '())
        (check-expect (drop 9 '(1 2 3 4 99)) '())
        (check-expect (drop 3 '()) '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 5


;; (zip xs ys) converts two lists into a list of pairs by associating
;;             corresponding values in two lists. xs and ys must be of
;;             same length

;; laws:
;;   (zip '() '()) == '()
;;   (zip x y) == '((x y)) (when x and y are singleton lists)
;;   (zip xs ys) == (cons (List2 (car xs) (car ys)) (zip (cdr xs) (cdr ys)))
;;                  (when xs and ys's length >= 2)
;; [optional notes about where laws come from, or difficulty, if any]

(define zip (xs ys)
    (if (null? xs)
        '()
        (if (null? (cdr xs))
            (cons (list2 (car xs) (car ys)) '())
            (cons (list2 (car xs) (car ys)) (zip (cdr xs) (cdr ys))))))

        (check-expect (zip '((a b) (c d) (e f)) '(1 2 3))
                           '(((a b) 1) ((c d) 2) ((e f) 3)))
        (check-expect (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))
        (check-expect (zip '(1) '(a)) '((1 a)))
        (check-expect (zip '() '()) '())




;; (unzip ps) converts a list of pairs into a pair of lists, each containing
;;            separate elements in the pairs. xs and ys must be of
;;            same length

;; laws:
;;   (unzip '()) == '()
;;   (unzip x) == '((caar x) (cadar x)) (when x consists of only one element)
;;   (unzip xs) == '((list of first elements from each pair)
;;                   (list of second elements from each pair))
;;                 (when xs has more than one pairs)
;; [optional notes about where laws come from, or difficulty, if any]

(define unzip (ps)
    (if (null? ps)
        '()
        (if (null? (cdr ps))
            (list2 (list1 (caar ps)) (list1 (cadar ps)))
            (list2 (map (lambda (x) (car x)) ps)
                   (map (lambda (y) (cadr y)) ps)))))

        (check-expect (unzip '((1 a) (2 b) (3 c))) '((1 2 3) (a b c)))
        (check-expect (unzip '((1 a))) '((1) (a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 6


;; (arg-max f xs) returns the x in non-empty list xs in which (f x) is the
;;                largest

;; laws:
;;   (arg-max f (cons a '())) == a
;;   (arg-max f (cons a as)) == a (when (f a) > (f (arg-max f as)))
;;                            (arg-max f as) (when (f a) <= (f (arg-max f as)))
;; [optional notes about where laws come from, or difficulty, if any]

(define arg-max (f xs)
    (if (null? (cdr xs))
        (car xs)
        (let ([curr (car xs)] [next (arg-max f (cdr xs))])
             (if (> (f curr) (f next))
                 curr
                 next))))

;; The following square function is for unit test only
        (define square (a) (* a a))
        (check-expect (arg-max square '(5 4 3 2 1 8 10)) 10)
        (check-expect (arg-max car '((105 PL) (160 Algorithms) (170 Theory)))
                      '(170 Theory))
