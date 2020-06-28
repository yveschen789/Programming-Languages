  (class NewFractionMethods
     [subclass-of Object]

     ; Definition of multiply
     (method * (anInt) [locals int]
        (set anInt (self coerce: anInt))
        (((Fraction new) setNum:den:  (num * (anInt num)) (den * (anInt den))) 
            divReduce))

      ; Definition of addition
      (method + (anInt) [locals temp]
         (set anInt (self coerce: anInt))
         (set temp (den lcm: (anInt den)))
         (((Fraction new) setNum:den:
                ((num * (temp div: den)) + ((anInt num) * (temp div: (anInt 
                                den)))) temp) divReduce))

      ; Definition of subtraction
      (method - (anInt)
        (self + (anInt negated) ))
  )

(Fraction addAllMethodsFrom: NewFractionMethods)

(val frac (Fraction num:den: 8 4))

(check-expect (frac - 1) (Fraction num:den: 1 1))

(check-expect (frac + 2) (Fraction num:den: 4 1))

(check-expect (frac * 4) (Fraction num:den: 8 1))
