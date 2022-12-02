; SAT Solver Test Cases

(record not [arg])
(record or  [args])
(record and [args])


(val f1 (make-or
            (list2 (make-and (list3 'x (make-not 'y) 'z))
                   (make-and (list3 (make-not 'x) 'y (make-not 'z))))))
(val s1 '((x #f) (y #t) (z #f)))

(val f2 (make-and
            (list6 (make-or (list3 'x 'y 'z))
                   (make-or (list3 (make-not 'x) (make-not 'y) (make-not 'z)))
                   (make-or (list3 (make-not 'x) 'y (make-not 'z)))
                   (make-or (list3 (make-not 'x) (make-not 'y) 'z))
                   (make-or (list3 'x (make-not 'y) 'z))
                   (make-or (list3 'x 'y (make-not 'z))))))
(val s2 '((x #f) (y #t) (z #t)))

(val f3 (make-and
            (list4 (make-or (list2 'x 'y))
                   (make-or (list2 (make-not 'x) (make-not 'y)))
                   (make-or (list2 'x (make-not 'y)))
                   (make-or (list2 (make-not 'x) 'y)))))
(val s3 'no-solution)
