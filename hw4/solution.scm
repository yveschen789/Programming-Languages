;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT ;;;;;;;;;;;;;;;
;; Yves Chen
;; Hofs HW4
;; solution.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 28

;; (max* xs) <find the maxinum of a non-empty list of integers>

(define max* (xs)
    (foldl max (car xs) (cdr xs)))

    (check-expect (max* '(1 2 3 4)) 4)
    (check-expect (max* '(7 1 2 3 4)) 7)

;; (sum xs) <find the sum of a non-empty list of integers>

(define sum (xs)
    (foldl + (car xs) (cdr xs)))

    (check-expect (sum '(1 2 3 4)) 10)
    (check-expect (sum '(7 1 2 3 4)) 17)

;; (product xs) <finds the product of non-empty list of integers>

(define product (xs)
    (foldl * (car xs) (cdr xs)))

    (check-expect (product '(1 2 3 4)) 24)
    (check-expect (product '(7 1 2 3 4)) 168)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 29

;; (append xs ys) <append two lists>

(define append (xs ys)
    (foldr cons ys xs))

    (check-expect (append '(1 2 3 4)  '(5 6 7 8)) '(1 2 3 4 5 6 7 8))
    (check-expect (append  '(5 6 7 8) '(a b c d)) '(5 6 7 8 a b c d))
    
;; (reverse xs ys) <reverses a list>

(define reverse (xs)
    (foldl cons '() xs))

    (check-expect (reverse '(5 6 7 8)) '(8 7 6 5))
    (check-expect (reverse '(a b c d)) '(d c b a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 30

;; (map f xs) <applies a function to every elt of list and returns new list>

(define map (f xs)
    (foldr (lambda (y ys) (cons (f y) ys)) '() xs))

(define add1 (x) (+ 1 x))
(define square (a) (* a a))

    (check-expect (map add1 '(1 2 3)) '(2 3 4))
    (check-expect (map square '(1 2 3)) '(1 4 9))

;; (filter f xs) <return list that satisfies the function>

(define filter (f xs)
    (foldr (lambda (y ys) (if (f y) (cons y ys) ys)) '() xs))

(define even? (x) (= (mod x 2) 0))
    
    (check-expect (filter even? '(3 4 5 6)) '(4 6))

;; (exists f xs) <checks if an element in the list satisfies the function>

(define exists? (f xs)
    (foldr (lambda (y ys) (if (null? y) #f (if (f y) #t ys))) #f xs))


    (check-expect (exists? even? '(3 4 5 6)) #t)
    (check-expect (exists? even? '( 1 1 3 4 5 5 7)) #t)
    (check-expect (exists? even? '( 1 1 3 9 5 5 7)) #f)
    (check-expect (exists? even? '( 2 1 3 9 5 5 7)) #t)
    (check-expect (exists? even? '( 1 1 3 9 5 5 8)) #t)

;; (all f xs) <find the maxinum of a non-empty list of integers>

(define all? (f xs)
    (foldr (lambda (y ys) (if (f y) ys #f)) #t xs))

    (check-expect (all? even? '(6 4 8 10)) #t)
    (check-expect (all? even? '( 4 4 4 4 4 4 4)) #t)
    (check-expect (all? even? '( 1 2 8 10 12 7)) #f)
    (check-expect (all? even? '( 1 2 8 10 12 7)) #f)
    (check-expect (all? even? '( 3 2 8 10 12 8)) #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 38


;; set evens, contains all even integers
(define evens (x) (= (mod x 2) 0))

(define member? (x s) (s x))

    (check-expect (member? 5 evens) #f)
    (check-expect (member? 6 evens) #t)

;; set two-digits, contains all two-digit positive numbers
(define two-digits (x) (and (< 9 x) (< x 100)))

    (check-expect (member? 8 two-digits) #f)
    (check-expect (member? 47 two-digits) #t)
    (check-expect (member? 99 two-digits) #t)


;;add-element, set of elements plus added element
(define add-element (x y) (lambda (z) (if (= x z) #t (member? z y))))

    (check-expect (member? 3 (add-element 3 evens)) #t)
    (check-expect (member? 33 (add-element 2 two-digits)) #t)

;; union, set containing elements of two diferent sets
(define union (x y) (lambda (z) (or (member? z x) (member? z y))))
    
    (check-expect (member? 12 (union two-digits evens)) #t)
    (check-expect (member? 11 (union two-digits evens)) #t)
    (check-expect (member? 7 (union two-digits evens)) #f)
    (check-expect (member? 102 (union evens two-digits)) #t)

;; inter, set containing elements that appear in two different sets
(define inter (x y) (lambda (z) (and (member? z x) (member? z y))))

    (check-expect (member? 12 (inter two-digits evens)) #t)
    (check-expect (member? 11 (inter two-digits evens)) #f)
    (check-expect (member? 6 (inter two-digits evens)) #f)
    (check-expect (member? 98 (inter evens two-digits)) #t)

;; diff, set containing elements of s1 but not also in s2
(define diff (x y) (lambda (z) (and (member? z x) (not (member? z y)))))

    (check-expect (member? 12 (diff two-digits evens)) #f)
    (check-expect (member? 11 (diff two-digits evens)) #t)

;; set-ops
(record set-ops [empty member? add-element union inter diff])

;; implement third order of polymorphism
(define set-ops-from (eq?)
  (let ([empty   (lambda (x) #f)]
        [member? (lambda (x s) (s x))]
        [add     (lambda (x y) (lambda (z) (or (eq? x z) (member? z y))))]
        [union   (lambda (x y) (lambda (z) (or (x z) (y z))))]
        [inter   (lambda (x y) (lambda (z) (and (x z) (y z))))]
        [diff    (lambda (x y) (lambda (z) (and (x z)  (not(y z)))))])
    (make-set-ops empty member? add union inter diff)))

;; unit tests
(check-assert (function? set-ops-from))
(check-assert (set-ops? (set-ops-from =)))

;; defining functions for unit tests
(val atom-set-ops (set-ops-from =))
(val atom-emptyset      (set-ops-empty atom-set-ops))
(val atom-member?      (set-ops-member? atom-set-ops))
(val atom-add-element  (set-ops-add-element atom-set-ops)) 
(val atom-union        (set-ops-union atom-set-ops))
(val atom-inter        (set-ops-inter atom-set-ops))
(val atom-diff         (set-ops-diff atom-set-ops))

;; part d unit tests
    (check-expect (atom-member? 5 evens) #f)
    (check-expect (atom-member? 6 evens) #t)
    (check-expect (atom-emptyset 8) #f)
    (check-expect (atom-member? 3 (atom-add-element 3 evens)) #t)
    (check-expect (atom-member? 12 (atom-union two-digits evens)) #t)
    (check-expect (atom-member? 11 (atom-inter two-digits evens)) #f)
    (check-expect (atom-member? 8 (atom-diff evens two-digits)) #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise F

;;(flip (f)) <takes two arg function as argument and returns same function with
;; args in different order>

;; (flip (f)) == (lambda (x y) (f y x))

(define flip (f) (lambda (x y) (f y x)))

    (check-expect ((flip <) 3 4) #f)
    (check-expect ((flip <=) 3 4) #f)
    (check-expect ((flip append) '(a b c) '(1 2 3)) '(1 2 3 a b c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise V

;;(faults/none ()) <returns a validator that takes a response and always 
;;  returns the empty list of faults, no matter what is in the response.>

;;(faults/none ()) == '()

(define faults/none ()
    (lambda (x) '()))
;; (faults/always (F)) <returns a validator that takes a response and always 
;; returns a singleton list containing the fault F, no matter what is in the 
;; response.>

;;(faults/always (F)) == (lambda (x) (list1 F))

(define faults/always (F)
    (lambda (x) (list1 F)))

(check-expect ((faults/always 'issue) '((3 10) (a bro) (5 12))) '(issue))

;; (faults/equal (k v)) <returns a validator that takes a response and if k is
;; bound to v in the response, returns a singleton list containing k. If k is
;; not bound to v in the response, the validator returns the empty list of
;; faults.>

;; (faults/equal (k v)) == (lambda (x) (if (= (find k x) v) (list1 k) '()))

(define faults/equal (k v)
    (lambda (x) (if (= (find k x) v) (list1 k) '())))

    (check-expect ((faults/equal 5 12) '((3 10) (a bro) (5 12))) '(5))

;;(faults/both (V1 V2)) <returns a new validator that combines checks from the 
;; two validators V1 and V2.> 
;; defines functions: member?, add-elem?, union

;; (faults/both (v1 v2))== (lambda (v1 v2) (lambda (x) (union (v1 x) (v2 x))))

(val faults/both 
   (let* ([member?  (lambda (x s) (exists? ((curry =) x) s))]
          [add-elem (lambda (x s) (if (member? x s) s (cons x s)))]
          [union    (lambda (fault1 fault2) (foldr add-elem fault2 fault1))])
       (lambda (v1 v2) (lambda (x) (union (v1 x) (v2 x))))))

(check-expect ((faults/both (faults/always 'issue) (faults/always 'issue2)) 
              '(mylist)) '(issue issue2)) 

(check-expect ((faults/both (faults/always 'issue) (faults/equal 5 12)) 
              '((3 10) (a bro) (5 12))) '(issue 5))

;; (faults/switch k t) <returns a validator that validates the response using a
;;validator selected from validator table t: the value of k in the response is 
;; a key used to find a validator in t.>

;; ((faults/switch (k t)) x) ==  ((find (find k x) t) x) 

(define faults/switch (k t)
    (lambda (x) ((find (find k x) t) x)))

(val regrade-validator  ;; example for the regrade form
  (faults/switch 'why
    (bind         'photo
                  (faults/none)
      (bind       'badsubmit
                   (faults/both (faults/equal 'badsubmit_asst '...)
                                (faults/equal 'info #f))
        (bind     'badgrade
                  (faults/both
                      (faults/equal 'badgrade_asst '...)
                      (faults/both
                         (faults/equal 'info #f)
                         (faults/equal 'problem #f)))
          (bind   'recitation
                  (faults/both
                      (faults/equal 'date #f)
                      (faults/equal 'leaders #f))

            (bind '#f
                  (faults/always 'nobutton)
                  '())))))))

(check-expect (regrade-validator '([why photo]))
              '())

(check-expect (regrade-validator '([why badsubmit]
                                  [info wrong-pdf]
                                  [badsubmit_asst opsem]))
              '())

(check-expect (regrade-validator '([why badsubmit]
                                  [badsubmit_asst opsem]
                                  [info #f]))
              '(info))

(check-expect (regrade-validator '([why badsubmit]
                                  [info wrong-pdf]
                                  [badsubmit_asst ...]))
              '(badsubmit_asst))

(check-expect (regrade-validator '([why badsubmit]
                                  [info #f]
                                  [problem #f]
                                  [badsubmit_asst ...]))
              '(badsubmit_asst info))

(check-expect (regrade-validator '([why badgrade]
                                  [info #f]
                                  [problem #f]
                                  [badgrade_asst impcore]))
              '(info problem))

;;(ordered-by? f) <that takes one argument—a comparison function that 
;;represents a transitive relation—and returns a predicate, precedes?, that 
;;tells if a list of values is totally ordered by that relation.>
;;defines function: precedes? <asserts front two elems of list of ordered
;; by predicate relation>

;; ((ordered-by? f) '()) == #t
;; ((ordered-by? f) y == #t
;; ((ordered-by? f) (cons x (cons y zs)) == (precedes? (cons y zs)) if (f x y)
;; ((ordered-by? f) (cons x (cons y zs)) == #f if not(f x y)

(define ordered-by? (f)
  (letrec([precedes? (lambda (xs)
    (if (null? xs)
      #t
      (if (atom? xs)
        #t
        (if (not(atom? (cdr xs)))
          (if (f (car xs) (cadr xs))
            (precedes? (cdr xs))
            #f)
          #t))))])
  precedes?))

(check-assert (function? ordered-by?))
(check-assert (function? (ordered-by? <)))
(check-error (ordered-by? < '(1 2 3)))
(check-assert ((ordered-by? <) '(1 2 3 4)))
(check-assert (not((ordered-by? <) '(3 2 1))))
    