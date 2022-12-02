;;;;;;;;;;;;;;;;;;; COMP 105 CONTINUATION ASSIGNMENT ;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Problem 2


;; (list-of? A? xs) takes a function which can be applied to any value and
;;                  returns a boolean, and any arbitrary value, this function
;;                  returns a boolean that indicates whether if v is a list of
;;                  values and each element of v satisfies A?
;; (list? x) takes any arbitrary uscheme value and returns true if x is a list
;;           of values and false otherwise.
;; laws
;;           (list-of? A? '()) == #t
;;   (list-of? A? (cons v vs)) == (and (A? v) (list-of? A? vs)
;;                             , when (cons v vs) is a list of values
;;             (list-of? A? v) == #f
;;                             , when v is not a list of values
;;                               this has none of the forms of the previous laws


(define list-of? (A? xs)
    (letrec ([list? (lambda (x) (if (pair? x)
                                    (list? (cdr x))
                                    (null? x)))])
        (if (null? xs)
             #t
             (if (not (pair? xs))
                  #f
                  (if (list? xs)
                      (and (A? (car xs)) (list-of? A? (cdr xs)))
                      #f)))))

        (define value? (_) #t) ;; for unit test, tell if argument is a value
        (define even? (x)
            (if (= 0 (mod x 2))
                 #t
                 #f)) ;; for unit test, return if x is even
        (check-assert (list-of? value? '(1 2 4 5)))
        (check-assert (list-of? value? '(#t #f #f #t)))
        (check-assert (not (list-of? number? (cons 'a 'b))))
        (check-assert (not (list-of? number? #t)))
        (check-assert (not (list-of? number? 1)))
        (check-assert (not (list-of? number? 'a)))
        (check-assert (not (list-of? number? symbol?)))
        (check-assert (list-of? number? '(1 2 4 5)))
        (check-assert (not (list-of? number? '(1 2 4 #t))))
        (check-assert (list-of? boolean? '(#t #f #f #t)))
        (check-assert (not (list-of? boolean? '(#t #f #f #t 2))))
        (check-assert (list-of? even? '(2 8 190)))
        (check-assert (not (list-of? even? '(2 7 190))))
        (check-assert (list-of? atom? '(2 #t)))
        (check-assert (not (list-of? atom? '(2 (2 9) 190))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Problem 3


;; (formula? fs) takes any arbitrary μscheme value and returns returns #t if
;;               the value represents a Boolean formula and #f otherwise

;; laws:
;; (formula? f) == #t ,when f is a symbol
;; (formula? (make-not f)) == #t ,when f is a formula
;; (formula? (make-and fs)) == #t ,when fs is a list of formulas
;; (formula? (make-or fs)) == #t ,when fs is a list formulas
;; (formula? v) == #f ,when v is none of the forms in previous laws
;;

;; Record definition for not, and, and or
(record not [arg])
(record or  [args])
(record and [args])

(define formula? (fs)
    (if (symbol? fs)
         #t
         (if (not? fs)
              (formula? (not-arg fs))
              (if (and? fs)
                   (list-of? formula? (and-args fs))
                   (if (or? fs)
                        (list-of? formula? (or-args fs))
                        #f)))))

        (check-assert (formula? 'x))
        (check-assert (not (formula? #t)))
        (check-assert (not (formula? 1)))
        (check-assert (not (formula? (cons 1 'a))))
        (check-assert (not (formula? (cons 1 '()))))
        (check-assert (formula? (make-not 'x)))
        (check-assert (formula? (make-not (make-and '(x y)))))
        (check-assert (formula? (make-not (make-or '(x y)))))
        (check-assert (not (formula? (make-not 1))))
        (check-assert (not (formula? (make-not #t))))
        (check-assert (not (formula? (make-not '(a 1)))))
        (check-assert (not (formula? (make-not '(a #t)))))
        (check-assert (not (formula? (make-not '(a null?)))))
        (check-assert (formula? (make-and '(x y))))
        (check-assert (not (formula? (make-and '(a 1)))))
        (check-assert (formula? (make-or '(x y))))
        (check-assert (not (formula? (make-or '(a 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Problem 4


;; (eval-formula f alist) takes a formula and an association list in which each
;;                        key is a symbol and each value is a Boolean and
;;                        returns whether f is satisfied in alist.
;; (and_key_list fs) takes a list of formulas and returns the result of 'and'
;;                   operation on what finding these formulas in alist returns.
;; (or_key_list fs) takes a list of formulas and returns the result of 'or'
;;                   operation on what finding these formulas in alist returns.

;; laws:
;;             (eval-formula f alist) == (find f alist) ,where f is a symbol
;;  (eval-formula (make-not f) alist) == (not (find f alist))
;; (eval-formula (make-and fs) alist) == (and (find (car fs) alist)
;;                                            (eval-formula (cdr fs) alist))
;;                                         ,where fs is a list of formulas
;;  (eval-formula (make-or fs) alist) == (or (eval-formula (car fs) alist)
;;                                           (eval-formula (cdr fs) alist))
;;                                         ,where fs is a list of formulas
;;

(define eval-formula (f alist)
    (if (symbol? f)
        (find f alist)
        (if (not? f)
            (not (find (not-arg f) alist))
            (if (and? f)
                (if (null? (and-args f))
                    #t
                    (and (eval-formula (car (and-args f)) alist)
                         (eval-formula (make-and (cdr (and-args f))) alist)))
                (if (or? f)
                    (if (null? (or-args f))
                        #f
                        (or (eval-formula (car (or-args f)) alist)
                            (eval-formula (make-or (cdr (or-args f))) alist)))
                    #f)))))

        ;; define env as an association for unit testing
        (val env (bind 'a #t '()))
        (val env (bind 'b #f env))
        (val env (bind 'c #f env))
        (val env (bind 'd #f env))
        (val env (bind 'product #t env))

        (check-assert (eval-formula 'a env))
        (check-assert (not (eval-formula 'b env)))
        (check-assert (eval-formula (make-not 'b) env))
        (check-assert (not (eval-formula (make-not 'product) env)))
        (check-assert (eval-formula (make-and '(a product)) env))
        (check-assert (not (eval-formula (make-and '(a product b)) env)))
        (check-assert (eval-formula (make-or '(a b c)) env))
        (check-assert (not (eval-formula (make-or '(b d)) env)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Problem 6

;; (find-formula-true-asst f fail succ) searches for an assignment that
;;            satisfies formula f. If it finds a satisfying assignment, it calls
;;            succ, passing both the satisfying assignment (as an association
;;            list) and a resume continuation. If it fails to find a satisfying
;;            assignment, it calls fail.

;; laws:
;; (find-formula-true-asst f             fail succ) == (fail)
;;                                                     , where f is not a symbol
;; (find-formula-true-asst f             fail succ) == (f-f-sym f #t '() fail
;;                                                                       succ)
;;                                                     , where f is a symbol
;; (find-formula-true-asst (make-and fs) fail succ) == (f-all-a fs #t '() fail
;;                                                                       succ)
;; (find-formula-true-asst (make-not fs) fail succ) == (f-f-sym fs #f '() fail
;;                                                                       succ)
;; (find-formula-true-asst (make-or fs)  fail succ) == (f-any-a fs #t '() fail
;;                                                                       succ)


(define find-formula-true-asst (f fail succ)
    (letrec (
;; (find-formula-symbol x bool cur fail succeed) takes five arguments, where x
;;          is a symbol, bool is a boolean value, cur is association list of
;;          current solution, fail is continuation that takes no argument, and
;;          succeed is continuation that takes two argument (current solution
;;          and resume continuation), it operates according to the following
;;          algebraic laws

;; laws:
;; (f-f-sym x bool cur fail succeed) == (succeed (bind x bool cur) fail)
;;                                      , where x is not bound in cur
;; (f-f-sym x bool cur fail succeed) == (succeed cur fail)
;;                                      , where x is bool in cur
;; (f-f-sym x bool cur fail succeed) == (fail)
;;                                      , where x is (not bool) in cur
             [find-formula-symbol
                 (lambda (x bool cur fail succeed)
                     (if (null? (find x cur))
                         (succeed (bind x bool cur) fail)
                         (if (= (find x cur) bool)
                             (succeed cur fail)
                             (fail))))]


;; (find-all-asst fs bool cur fail succeed) takes five arguments, where fs is a
;;          list of formulas, bool is a boolean value, cur is association list
;;          of current solution, fail is continuation that takes no argument,
;;          succeed is continuation that takes two argument (current solution
;;          and resume continuation), it extends cur to find an assignment
;;          that makes every formula in the list fs equal to bool

;; laws:
;;         (f-all-a '() bool cur fail succeed) == (succeed cur fail)
;; (f-all-a (cons f fs) bool cur fail succeed) == (f-f-a f bool cur fail (lambda
;;                         (cur' resume) (f-all-a fs bool cur' resume succeed)))
             [find-all-asst
                 (lambda (x bool cur fail succeed)
                     (if (null? x)
                         (succeed cur fail)
                         (find-formula-asst (car x) bool cur fail
                                            (lambda (new_cur resume)
                                                    (find-all-asst (cdr x) bool
                                                                   new_cur
                                                                   resume
                                                                   succeed)))))]

;; (f-any-a fs bool cur fail succeed) takes five arguments, where fs is a list
;;          of formulas, bool is a boolean value, cur is association list of
;;          current solution, fail is continuation that takes no argument, and
;;          succeed is continuation that takes two argument (current solution
;;          and resume continuation), it extends cur to find an assignment
;;          that makes any one of formulas in the list fs equal to bool

;; laws:
;;         (f-any-a '() bool cur fail succeed) == (fail)
;; (f-any-a (cons f fs) bool cur fail succeed) == (f-f-a f bool cur (lambda ()
;;                                  (f-any-a fs bool cur fail succeed)) succeed)
              [find-any-asst
                  (lambda (x bool cur fail succeed)
                      (if (null? x)
                          (fail)
                          (find-formula-asst (car x) bool cur
                                             (lambda ()
                                                 (find-any-asst (cdr x) bool cur
                                                                fail succeed))
                                             succeed)))]
;; (find-formula-asst f bool cur fail succeed) takes five arguments, where fs is
;;          a formula, bool is a boolean value, cur is association list of
;;          current solution fail is continuation that takes no argument,
;;          succeed is continuation that takes two argument (current solution
;;          and resume continuation), it extends cur to find an assignment that
;;          makes the single formulas f equal to bool

;; laws:
;;          (f-f-a x bool cur fail succeed) == (f-f-sym x bool cur fail succeed)
;;                                                , where x is a symbol
;;(f-f-a (make-not f) bool cur fail succeed) == (f-f-a f (not bool) cur fail
;;                                                                      succeed)
;; (f-f-a (make-or  fs) #t cur fail succeed) == (f-any-a fs #t cur fail succeed)
;; (f-f-a (make-or  fs) #f cur fail succeed) == (f-all-a fs #f cur fail succeed)
;; (f-f-a (make-and fs) #t cur fail succeed) == (f-all-a fs #t cur fail succeed)
;; (f-f-a (make-and fs) #f cur fail succeed) == (f-any-a fs #f cur fail succeed)
               [find-formula-asst
                   (lambda (x bool cur fail succeed)
                       (if (symbol? x)
                           (find-formula-symbol x bool cur fail succeed)
                           (if (not? x)
                               (find-formula-asst (not-arg x) (not bool) cur
                                                  fail succeed)
                               (if (and? x)
                                   (if bool
                                       (find-all-asst (and-args x) #t cur fail
                                                      succeed)
                                       (find-any-asst (and-args x) #f cur fail
                                                      succeed))
                                   (if bool
                                       (find-any-asst (or-args x) #t cur fail
                                                      succeed)
                                       (find-all-asst (or-args x) #f cur fail
                                                      succeed))))))])
        (find-formula-asst f #t '() fail succ)))

        (use solver-interface-tests.scm)
        (use solver-tests.scm)
    ;; one-solution evaluates a formula and returns one solution of assignment
    ;; defined for unit testing
        (define one-solution  (fs)
            (find-formula-true-asst fs
                                (lambda () 'no-solution)
                                (lambda (cur resume) cur)))

        (check-expect
           (find-formula-true-asst f1
                                   (lambda () 'fail)
                                   (lambda (cur resume) 'succeed))
           'succeed)
        (check-expect
           (find-formula-true-asst f2
                                   (lambda () 'fail)
                                   (lambda (cur resume) 'succeed))
           'succeed)
        (check-expect
           (find-formula-true-asst f3
                                   (lambda () 'no-solution)
                                   (lambda (cur resume) cur))
           s3)
