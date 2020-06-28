;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT ;;;;;;;;;;;;;;;
;; Yves Chen
;; Scheme HW3
;; solution.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 1


;; (contig-sublist? xs ys) <determines if xs is a contiguous subsequence of ys>

;; laws:
;;   (sublisthelper '() ys) == #t
;;   (sublisthelper (cons y zs) (cons y xs)) == (sublisthelper zs xs)
;;   (sublisthelper (cons z zs) (cons x sx)) == #f

;; checks is xs is equivalent to prefix of ys
(define sublisthelper (xs ys)
    (if (null? xs)
    #t
    (if (= (car xs) (car ys))
        (sublisthelper (cdr xs) (cdr ys))
        #f)))

    (check-assert (sublisthelper '(a b c) '(a b c)))
    (check-assert (not(sublisthelper '(a d c) '(a b c d))))

(define contig-sublist? (xs ys)
    (if (null? ys)
    #f
    (|| (sublisthelper xs ys) (contig-sublist? xs (cdr ys)))))


        (check-assert (not(contig-sublist? '(a b c) '(x a y b z c))))
        (check-assert (contig-sublist? '(a y b) '(x a y b z c)))
        (check-assert (contig-sublist? '(x) '(x a y b z c)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 8


;; (mirror xs) <returns a list in which every list in xs is recursively 
;; mirrored and the resulting lists are in reverse order> 

;; laws:
;;   (mirror '()) == '()
;;   (mirror (cons y ys)) == (append (mirror(ys) list1(y)) y is an atom
;;   (mirror (cons y ys)) == (append (mirror(ys) list1(mirror(y)) y is a list



(define mirror (xs)
    
    (if (null? xs)
        xs
        (if (atom? (car xs))
            (append (mirror (cdr xs))(list1 (car xs)))
            (append (mirror (cdr xs))(list1 (mirror (car xs)))))))
        

        (check-expect (mirror '(1 2 3 4 5)) '(5 4 3 2 1))
        (check-expect (mirror '(1 (2 3) 4 (6 5))) '((5 6) 4 (3 2) 1))



;; (flatten xs) <consumes list and erases internal parentheses>

;; laws:
;;   (flatten '())) == '() 
;;   (flatten a) == (list1 a)
;;   (flatten (cons y ys)) == (append (flatten y) (flatten ys))  


(define flatten (xs)
    (if (null? xs)
        '()
        (if (atom? xs)
            (list1 xs)
            (append (flatten (car xs)) (flatten (cdr xs))))))

        
        (check-expect (flatten '((I Ching)(U Thant)(E Coli))) 
        '(I Ching U Thant E Coli))
        (check-expect (flatten '((((a))))) '(a))
        (check-expect (flatten '()) '())
        (check-expect (flatten '((a) () ((b c) d e))) '(a b c d e))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 31


;; (takewhile p? xs) <returns longest prefix list of values of xs that satisfy 
;;  p?>

;; laws:
;;   (takewhile p? '()) == '()
;;   (takewhile #t (cons y ys)) == (append (list1 y) (takewhile p? ys))
;;   (takewhile #f (cons y ys)) == '()

(define even? (x) (= (mod x 2) 0))

(define takewhile (p? xs)
    (if (null? xs)
        '()
        (if (p? (car xs))
            (append (list1 (car xs)) (takewhile p? (cdr xs)))
            '())))

        (check-expect (takewhile even? '(2 4 6 7 8 10 12)) '(2 4 6))
        (check-expect (takewhile even? '(2 5 6 7 8 10 12)) '(2))
        (check-expect (takewhile even? '(3 4 6 4 8 11 12)) '())
        (check-expect (takewhile even? '(2 4 6 8 8 10 12)) '(2 4 6 8 8 10 12))



;; (dropwhile p? xs) <returns new list after removing longest prefix values 
;;  that satisfy p? in xs>

;; laws:
;;   (dropwhile p? '()) == '()
;;   (dropwhile #t (cons y ys)) == (dropwhile p? ys)
;;   (dropwhile #f (cons y ys)) == (ys)


(define dropwhile (p? xs)
    (if (null? xs)
        '()
        (if (p? (car xs))
            (dropwhile p? (cdr xs))
            xs)))

        (check-expect (dropwhile even? '(2 4 6 7 8 10 12)) '(7 8 10 12))
        (check-expect (dropwhile even? '(2 5 6 7 8 10 12)) '(5 6 7 8 10 12))
        (check-expect (dropwhile even? '(3 4 6 4 8 11 12)) '(3 4 6 4 8 11 12))
        (check-expect (dropwhile even? '(2 4 6 8 8 10 12)) '())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise B


;; (take n xs) <returns the longest prefix of xs that contains at most n elts>

;; laws:
;;   (take n '()) == '()
;;   (take 0 (cons y ys)) == '()
;;   (take n (cons y ys)) == (append list1(y) (take n-1 ys)) 

(define take (n xs)
    (if (null? xs)
        '()
        (if (> n 0)
            (append (list1 (car xs)) (take (- n 1) (cdr xs)))
            '())))

        (check-expect (take 4 '(2 4 6 7 8 10 12)) '(2 4 6 7))
        (check-expect (take 5 '(2 4 6 7 8 10 12)) '(2 4 6 7 8))
        (check-expect (take 0 '(2 4 6 7 8 10 12)) '())
        (check-expect (take 4 '()) '())
        (check-expect (take 1 '(256 4435 665 7 8 10 12)) '(256))



;; (drop n xs) <removes n elts from front of list and returns new list>

;; laws:
;;   (drop n '()) == '()
;;   (drop 0 (cons y ys)) == ys
;;   (drop n (cons y ys)) == (drop n-1 ys)

(define drop (n xs)
    (if (null? xs)
        '()
        (if (> n 0)
            (drop (- n 1) (cdr xs))
            xs)))

        (check-expect (drop 4 '(2 4 6 7 8 10 12)) '(8 10 12))
        (check-expect (drop 5 '(2 4 6 7 8 10 12)) '(10 12))
        (check-expect (drop 0 '(2 4 6 7 8 10 12)) '(2 4 6 7 8 10 12))
        (check-expect (drop 4 '()) '())
        (check-expect (drop 1 '(256 4435 665 7 8 10 12)) '(4435 665 7 8 10 12))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise C


;; (zip xs ys) <converts pair of lists to a list of pairs>

;; laws:
;;   (zip (cons x xs) (cons y ys)) == (cons (list2 x y) (zip xs ys))
;;   (zip (cons x '()) (cons y '())) == '()

(define zip (xs ys)
    (if (or (null? xs) (null? ys))
        '() 
        (cons (list2 (car xs) (car ys)) (zip (cdr xs) (cdr ys)))))

        (check-expect (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))
        (check-expect (zip '(11 11 15) '(Guyer Sheldon Korman)) 
        '((11 Guyer) (11 Sheldon) (15 Korman)))



;; (unzip ps) <converts a list of pairs to a pair of lists>

;; laws:
;;   (unzip '()) == '(() ())
;;   (unzip (cons y ys)) == (list2 (getkey (cons y ys))(getvalue (cons y ys)))
;; [optional notes about where laws come from, or difficulty, if any]

;; recursively returns a list of the first value of a list of pairs
(define getkey (xs)
    (if (null? xs)
        '()
        (cons (caar xs) (getkey (cdr xs)))))

;; recursively returns a list of the second value of a list of pairs
(define getvalue (xs)
    (if (null? xs)
        '()
        (cons (cadar xs) (getvalue (cdr xs)))))


(define unzip (ps)
    (if (null? ps)
        '(() ())
        (list2 (getkey ps) (getvalue ps)))) 

        
        (check-expect (unzip '((1 2) (3 4) (5 6))) '((1 3 5) (2 4 6)))
        (check-expect (unzip '((11 Guyer) (11 Sheldon) (15 Korman)))
        '((11 11 15) (Guyer Sheldon Korman)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise D


;; (arg-max f xs) <takes two args and returns elt in xs where (f x) is largest
;;  of elts in list xs as parameters>

;; laws:
;;   (arg-max f (cons x xs)) == (arg-max-helper f xs x)
;;
;;  (arg-max-helper f (cons y ys) x) == (arg-max-helper f ys y) (f y) > (f x)
;;  (arg-max-helper f (cons y ys) x) == (arg-max-helper f ys x) (f x) > (f y)

;; takes three args and returns elt in xs (the third param) where (f x) is 
;; largest of elts in list xs as parameters

(define square (a) (* a a))

(define arg-max-helper (f xs x)
    (if (null? xs)
        x
        (if (< (f x) (f (car xs)))
            (arg-max-helper f (cdr xs) (car xs))
            (arg-max-helper f (cdr xs) x ))))

(check-expect (arg-max-helper square '(5 4 3 2 1) 5) 5)
(check-expect (arg-max-helper square '(2 4 3 5 1) 2) 5)

(define arg-max (f xs)
    (arg-max-helper f (cdr xs) (car xs)))



        ;; replace next line with good check-expect or check-assert tests
        (check-expect (arg-max square '(5 4 3 2 1)) 5)
        (check-expect (arg-max car '((105 PL) (160 Algorithms) (170 Theory)))
        '(170 Theory))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise E


;; (rightmost-point ps) <returns point record with largest x coordinate>


(record point [x y])

(define rightmost-point (ps)
    (arg-max point-x ps)) 
        
        
        (check-expect (rightmost-point '((make-point 2 3) (make-point 4 5) 
            (make-point 1 2))) (make-point 4 5))

        (check-expect (rightmost-point '((make-point 10 0) (make-point 124 5) 
            (make-point 1 2))) (make-point 124 5))