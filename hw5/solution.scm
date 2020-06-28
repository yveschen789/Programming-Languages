;;;;;;;;;;;;;;;;;;; COMP 105 Continuations ASSIGNMENT ;;;;;;;;;;;;;;;
;; Yves Chen
;; Continuations HW5
;; solution.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise L


;; (list-of (A? v) <eturns a Boolean that is #t if v is a list of values, each
;;  of which satisfies A?. Otherwise, (list-of? A? v) returns #f.>)

;; (list-of? A? '()) == (A? '())
;; (list-of? A? y) == #f
;; (list-of? A? (cons y ys)) == (and (A? y) (list-of? A? ys)
;; (list-of A? (cons y '())) == (A? y)
;; (list-of? A? '()) == (A? '())
;; (list-of? A? y) == #f
;; else #f


(define list-of? (A? v)
    (if (null? v)
        (A? v)
        (if (or (or (function? v) (symbol? v)) (or (number? v) (boolean? v)))
            #f
            (if (pair? v)
                (if (null? (cdr v))
                    (A? (car v))
                    (and (A? (car v)) (list-of? A? (cdr v))))                
                #f))))



(define value? (_) #t)
(define even? (x) (= (mod x 2) 0))

    (check-assert (list-of? value? '()))
    (check-assert (not(list-of? value? (cons 1 2))))
    (check-assert (list-of? value? '(#t #f 105)))
    (check-assert (not(list-of? value? 3)))
    (check-assert (list-of? value? '(+)))
    (check-assert (list-of? value? '(+ 38 105)))
    (check-assert (list-of? value? '(+ list-of? 105)))
    (check-assert (list-of? even? '(8 10 122)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise F

(record not [arg])
(record or  [args])
(record and [args])


;;(formula? v) <which when given an arbitrary μScheme value, returns #t if the
;;  value represents a Boolean formula and #f otherwise>


;; (formula? v) == #t when x is a symbol
;; (formula? (make-or vs)) == (list-of? formula? (or-args vs))
;; (formula? (make-and vs)) == (list-of? formula? (and-args vs))
;; (formula? (make-not v)) == (formula? (not-arg v))
;; else #f

(define formula? (v)
    (cond
        [(symbol? v) #t]
        [(not? v) (formula? (not-arg v))]
        [(or? v) (list-of? formula? (or-args v))]
        [(and? v) (list-of? formula? (and-args v))]
        [#t #f]))

(check-assert (formula? (make-not 'A)))
(check-assert (not(formula? '(6 10 12))))
(check-assert (not(formula? (cons 1 2))))
(check-assert (formula? (make-and (list2 'a 'b))))
(check-assert (formula? (make-and 
                            (list2 'w (make-or (list3 'w 'y 'z ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise E


;; (eval-formula (f env)) <takes a formula arg and an environment and
;; returns a bool representing if the formula is satisfied by the env>


;; (map2 f xs env) <applies function f to every elt in list xs: f takes
;; two args, each element from xs and an unaltered parameter env>
(define map2 (f xs env)
    (foldr (lambda (y ys) (cons (f y env) ys)) '() xs))

;; (true? x) <return boolean value of boolean input x>
(define true? (x)
    x)

;; (eval-formula f env) == (find f env) if f is a symbol
;; (eval-formula make-not env) == (not(eval-formula (not-arg make-not) env))
;; (eval-formula make-or env) == (exists? t 
;;                               (map eval-formula (or-args make-or)))
;; (eval-formula make-and env) == (all? t 
;;                                  (map eval-formula (and-args make-and)))


(define eval-formula (f env)
    (cond
        [(symbol? f) (find f env)]
        [(not? f) (not(eval-formula (not-arg f) env))]
        [(or? f) (exists? true? (map2 eval-formula (or-args f) env))]
        [(and? f) (all? true? (map2 eval-formula (and-args f) env))]
        [#t (println f)]))

(val env (bind 'A #f (bind 'B #t (bind 'x #f (bind 'w #t '())))))

(check-expect (eval-formula (make-and (list2  'A 'B))
    env) #f)

(check-expect (eval-formula (make-and (list3  (make-not 'A)  (make-not 'x)  
    'B )) env) #t)

(check-expect (eval-formula (make-or (list2 (make-and (list2 'x 'A)) 
    (make-not 'w))) env) #f)

(check-expect (eval-formula (make-or (list2 (make-and (list2 'x 'w)) 
    (make-not 'B))) env) #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise S


;; (find-formula-true-asst (f fail succ)) <searches for an assignment that 
;; satisfies formula f. If it finds a satisfying assignment, it calls succ, 
;; passing both the satisfying assignment (as an association list) and a resume
;; continuation. If it fails to find a satisfying assignment, it calls fail.>


;; (find-formula-asst x            bool cur fail succeed) == 
;;  (find-formula-symbol x bool cur fail succeed)    where x is a symbol
;; (find-formula-asst (make-not f)  bool cur fail succeed) == 
;;  (find-formula-asst f (not bool) cur fail succeed)
;; (find-formula-asst (make-or fs) #t   cur fail succeed) == 
;;  (find-any-asst fs #t cur fail succeed)
;; (find-formula-asst (make-or  fs) #f   cur fail succeed) == 
;;  (find-all-asst fs #f cur fail succeed)
;; (find-formula-asst (make-and fs) #t   cur fail succeed) == 
;;  (find-all-asst fs #t cur fail succeed)
;; (find-formula-asst (make-and fs) #f   cur fail succeed) == 
;;  (find-any-asst fs #f cur fail succeed)
;;
;; (find-all-asst '()         bool cur fail succeed) == (succeed cur fail)
;; (find-all-asst (cons f fs) bool cur fail succeed) == 
;;  (find-formula-asst f bool cur fail (lambda (cur resume) find-all-asst
;;                                       fs bool cur resume succeed))
;;
;; (find-any-asst '()         bool cur fail succeed) == (fail)
;; (find-any-asst (cons f fs) bool cur fail succeed) == 
;;  (find-formula-asst f bool cur (lambda () find-any-asst fs bool curr
;;                                  fail succeed) succeed)
;;
;; (find-formula-symbol x bool cur fail succeed) == where x is not bound in cur
;; (succeed (bind x bool cur) fail)
;; (find-formula-symbol x bool cur fail succeed) == where x is bool in cur
;;  (succeed cur fail)
;;(find-formula-symbol x bool cur fail succeed) == where x is (not bool) in cur
;;  (fail)

(define find-formula-true-asst (f fail succ)
    (letrec 
        ([find-formula-asst          
                (lambda (x bool cur fail succeed)
                    (cond
                        [(symbol? x) 
                            (find-formula-symbol x bool cur fail succeed)]
                        [(not? x) (find-formula-asst 
                                    (not-arg x) (not bool) cur fail succeed)]
                        [(or? x) (if (= bool #t)
                                    (find-any-asst 
                                        (or-args x) bool cur fail succeed)
                                    (find-all-asst 
                                        (or-args x) bool cur fail succeed))]
                        [(and? x) (if (= bool #t)
                                    (find-all-asst 
                                        (and-args x) bool cur fail succeed)
                                    (find-any-asst 
                                        (and-args x) bool cur fail succeed))]))
        ]


        [find-all-asst 
            (lambda (x bool cur fail succeed)
                (if (null? x)
                    (succeed cur fail)
                    (find-formula-asst (car x) bool cur fail 
                                (lambda (cur resume) (find-all-asst
                                   (cdr x) bool cur resume succeed)))))
        ]


        [find-any-asst 
            (lambda (x bool cur fail succeed)
                (if (null? x)
                    (fail)
                    (find-formula-asst (car x) bool cur (lambda () 
                                (find-any-asst 
                                    (cdr x) bool curr fail succeed)) succeed)))
        ]


        [find-formula-symbol 
            (lambda (x bool cur fail succeed)
                (if (null? (find x cur))
                    (succeed (bind x bool cur) fail)
                    (if (= bool (find x cur))
                        (succeed cur fail)
                        (fail))))

        ])
    (find-formula-asst f #t '() fail succ)))

(check-assert (function? find-formula-true-asst))    ; correct name
(check-error (find-formula-true-asst))                ; not 0 arguments
(check-error (find-formula-true-asst 'x))             ; not 1 argument
(check-error (find-formula-true-asst 'x (lambda () 'fail)))   ; not 2 args
(check-error
   (find-formula-true-asst 'x (lambda () 'fail) (lambda (c r) 
                                'succeed) 'z)) ; not 4 args

(check-error (find-formula-true-asst 'x (lambda () 'fail) 
                                        (lambda () 'succeed)))
    ; success continuation expects 2 arguments, not 0
(check-error (find-formula-true-asst 'x (lambda () 'fail) 
                                        (lambda (_) 'succeed)))
    ; success continuation expects 2 arguments, not 1
(check-error (find-formula-true-asst
                   (make-and (list2 'x (make-not 'x)))
                   (lambda (_) 'fail)
                   (lambda (_) 'succeed)))
    ; failure continuation expects 0 arguments, not 1

    
    (check-assert (function? find-formula-true-asst))    ; correct name
    (check-error (find-formula-true-asst))                ; not 0 arguments
    (check-error (find-formula-true-asst 'x))             ; not 1 argument
    (check-error (find-formula-true-asst 'x (lambda () 'fail)))   ; not 2 args
    (check-error
       (find-formula-true-asst 'x (lambda () 'fail) 
                                    (lambda (c r) 'succeed) 'z)) ; not 4 args

    (check-error (find-formula-true-asst 'x (lambda () 'fail) 
                                            (lambda () 'succeed)))
        ; success continuation expects 2 arguments, not 0
    (check-error (find-formula-true-asst 'x (lambda () 'fail) 
                                            (lambda (_) 'succeed)))
        ; success continuation expects 2 arguments, not 1
    (check-error (find-formula-true-asst
                       (make-and (list2 'x (make-not 'x)))
                       (lambda (_) 'fail)
                       (lambda (_) 'succeed)))
        ; failure continuation expects 0 arguments, not 1


    (check-expect   ; x can be solved
       (find-formula-true-asst 'x
                               (lambda () 'fail)
                               (lambda (cur resume) 'succeed))
       'succeed)

    (check-expect   ; x is solved by '((x #t))
       (find-formula-true-asst 'x
                               (lambda () 'fail)
                               (lambda (cur resume) (find 'x cur)))
       #t)

    (check-expect   ; (make-not 'x) can be solved
       (find-formula-true-asst (make-not 'x)
                               (lambda () 'fail)
                               (lambda (cur resume) 'succeed))
       'succeed)

    (check-expect   ; (make-not 'x) is solved by '((x #f))
       (find-formula-true-asst (make-not 'x)
                               (lambda () 'fail)
                               (lambda (cur resume) (find 'x cur)))
       #f)

    (check-expect   ; (make-and (list2 'x (make-not 'x))) cannot be solved
       (find-formula-true-asst (make-and (list2 'x (make-not 'x)))
                               (lambda () 'fail)
                               (lambda (cur resume) 'succeed))
       'fail)






