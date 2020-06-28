;; Yves Chen


 (check-type drop (forall ('a) (int (list 'a) -> (list 'a))))


(val drop
    (type-lambda ['a]
        (letrec ([[drop-mono : (int (list 'a) -> (list 'a))]
                    (lambda ([n : int] [xs : (list 'a)])
                        (if ((@ null? 'a) xs)
                            (@ '() 'a)
                            (if (> n 0)
                                (drop-mono (- n 1) ((@ cdr 'a) xs))
                                xs)))])
        drop-mono)))




(check-type takewhile (forall ('a) (('a -> bool) (list 'a) -> (list 'a))))

(val takewhile
    (type-lambda ['a]
        (letrec ([[takewhile-mono : (('a -> bool) (list 'a) -> (list 'a))]
                    (lambda ([p? : ('a -> bool)] [xs : (list 'a)])
                        (if ((@ null? 'a) xs)
                            (@ '() 'a)
                            (if (p? ((@ car 'a) xs))
                                ((@ cons 'a) ((@ car 'a) xs) (takewhile-mono  
                                                    p? ((@ cdr 'a) xs)))

                                (@ '() 'a))))])
        takewhile-mono)))