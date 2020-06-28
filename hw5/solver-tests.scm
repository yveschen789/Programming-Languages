;;;;;;;;;;;;;;;;;;; COMP 105 Continuations ASSIGNMENT ;;;;;;;;;;;;;;;
;; Yves Chen
;; Continuations HW5
;; solver-tests.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Template for SAT Solver Test Cases 

(record not [arg])   ;; OK if these are duplicates
(record or  [args])
(record and [args])


; Makes sure continuation in the success pass continues without altering env.
; when encountering a new symbol, solver has no problem adding it to env.
(val f1 (make-or (list3 (make-and (list2 'x 'y))
                        (make-and (list2 'x 'y))
                        (make-not 'z))))
(val s1 '((x #t)(y #t)(z #f)))

; Although w can not be assigned a value to satisfy the and, the and is
; preceded by an or and therefore even if one value is unassignable only
; one of the or elements in the list needs to be true to satisfy
(val f2 (make-or(list2 (make-and (list2 'w (make-not 'w)))
                        x')))
(val s2 '(x #t))

; first inner and condition can be solved but second and condition cannot.
; therefore there is no solution
(val f3 (make-and(list2 (make-and(list2 'x (make-not 'w))) 
                        (make-and(list2 'x 'w)))))
(val s3 'no solution' )

