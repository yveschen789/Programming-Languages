(* Yves Chen *)
(* Core ML *)
(* warmup.sml *)

exception Match
exception Mismatch

(***** Problem A *****)

(*mynull : 'a list -> bool <checks if list is empty>*)

fun mynull [] = true
  | mynull (x::xs) = false

(***** Problem B *****)

(*reverse : 'a list -> 'a list <reverses a list>*)

fun reverse [] = []
  | reverse (x::xs) = foldl (fn (l, a) => l::a) [] (x::xs)

        val () =
            Unit.checkExpectWith (Unit.listString Unit.intString) "reversing"
            (fn () => reverse [1, 2, 3])
            [3, 2, 1]

(*minlist : int list -> int <returns smallest elt in a non-empty list>*)

fun minlist [] = raise Match
  | minlist (x::xs) = foldl (fn (l, a) => if l > a then a else l) x xs

(***** Problem C *****)

(*zip: 'a list * 'b list -> ('a * 'b) list* <zips two lists into a list of 
                                                tuples by element>*)

fun zip ([], []) = []
  | zip ((x::xs), (y::ys)) = (x, y)::zip (xs, ys)
  | zip ([], x) = raise Mismatch
  | zip (x, []) = raise Mismatch

        val () =
            Unit.checkExpectWith (Unit.listString 
                (Unit.pairString Unit.intString Unit.stringString)) "zip1"
            (fn() => zip ([1, 5, 9], ["a", "b", "c"]) )
            [(1, "a"), (5, "b"), (9, "c")] 


(***** Problem D *****)

(*pairfoldrEq : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
  <applies three-arg function to a pair of lists of equal length>*)

 fun pairfoldrEq f acc ([], []) = acc
   | pairfoldrEq f acc (x::xs, y::ys) = f ( x, y, pairfoldrEq f acc (xs, ys))
   | pairfoldrEq f acc _ = raise Mismatch 

(*ziptoo : 'a list * 'b list -> ('a * 'b) list <equal to zip but uses
                                                pairfoldrEq>*)

fun ziptoo ([], []) = []
  | ziptoo ((x::xs), (y::ys)) = pairfoldrEq (fn (a,b,c) => (a, b):: c) 
                                                    [] (x::xs, y::ys)
  | ziptoo (_, _) = raise Mismatch


        val () =
            Unit.checkExpectWith (Unit.listString 
                (Unit.pairString Unit.intString Unit.stringString)) "zip2"
            (fn() => ziptoo ([1, 5, 9], ["a", "b", "c"]) )
            [(1, "a"), (5, "b"), (9, "c")] 


(***** Problem E *****)

(*concat : 'a list list -> 'a list <takes list of lists and returns
                                        a single lst of all elts>*)

fun concat [] = []
  | concat (x::xs) = x @ concat xs

        val () =
            Unit.checkExpectWith (Unit.listString Unit.intString)
             "concat"
            (fn() => concat [[1], [2, 3, 4], [], [5, 6]])
            [1, 2, 3, 4, 5, 6]

(***** Problem F *****)


datatype sx
  = SYMBOL of string
  | NUMBER of int
  | BOOL   of bool
  | SXLIST of sx list

(*numbersSx : int list -> sx <takes a list of nums and creates s expression>*)

fun numbersSx [] = SXLIST[]
  | numbersSx xs = SXLIST(map NUMBER xs)

(*flattenSyms : sx -> string list <extracts symbols from s expression>*)

fun flattenSyms (SYMBOL sx) = [sx]
  | flattenSyms (NUMBER _) = []
  | flattenSyms (BOOL _) = []
  | flattenSyms (SXLIST sxs) = List.concat (List.map flattenSyms sxs)


(***** Problem G *****)


type 'a env = string -> 'a

exception NotFound of string

(*allZeroes : int env <returns env that maps string to zero >*)

val allZeroes = (fn s => 0)

(*emptyEnv : 'a env <takes string and returns an exeption not found>*)

val emptyEnv = (fn s => raise NotFound s)

(*bindVar : string * 'a * 'a env -> 'a env <adds string and 'a to an env>*)

fun bindVar (str : string, a, env) : 'a env = 
                                    (fn (s) => if s = str then a else env s) 

(*lookup  : string * 'a env -> 'a <checks if a string in an env returns 'a>*)

fun lookup (str: string, env) = env str

(*isBound : string * 'a env -> bool <checks if string exists in an env>*)

fun isBound (str : string, env : 'a env) =  (lookup(str, env); true)
                                             handle (NotFound str) => false



(***** Problem H *****)

datatype nat = ZERO
             | TIMES10PLUS of nat * int


fun times10plus (ZERO, 0) = ZERO
  | times10plus (m, d)    = TIMES10PLUS (m, d)

(* times10 : nat -> nat *)
fun times10 n = times10plus (n, 0)

(* natOfDigit : int -> nat *)
fun natOfDigit d = times10plus (ZERO, d)

fun flip f (x, y) = f (y, x)

(* natOfDigits : int list -> nat *)
fun natOfDigits ds = foldl (flip times10plus) ZERO ds


fun rawNatString ZERO = "ZERO"
  | rawNatString (TIMES10PLUS (m, d)) = 
      "(" ^ rawNatString m ^ " * 10 + " ^ Int.toString d ^ ")"

(*intOfNat : nat -> int <converts an int to a natural number>*)

fun intOfNat ZERO = 0
  | intOfNat (TIMES10PLUS (natu, inte)) = (intOfNat natu) * 10 + inte

        
        val () =
            Unit.checkExpectWith Unit.intString "intOfNat"
            (fn() => intOfNat (natOfDigits [1, 2, 3]))
            123

(*natOfInt : int -> nat <takes a naturan number and returns an int>*)

fun natOfInt 0 = ZERO
  | natOfInt inte = times10plus(natOfInt (inte div 10), inte mod 10)


        val () =
            Unit.checkExpectWith Unit.stringString "nat of int"
            (fn() => rawNatString (natOfInt 12))
            "((ZERO * 10 + 1) * 10 + 2)"

(*natString : nat -> string <converts a natural number to a string>*)

fun natString ZERO = Int.toString 0
  | natString (TIMES10PLUS(ZERO, inte)) = Int.toString inte
  | natString (TIMES10PLUS(natu, inte)) = (natString natu) ^ 
                                            (Int.toString inte)

        val () =
            Unit.checkExpectWith Unit.stringString "natstring"
            (fn() => natString (natOfInt 2018))
            "2018"

  


(***** Problem I *****)

exception notMachine

(*carryIntoNat : nat * int -> nat <adds a machine bit to a nat to return>*)

fun carryIntoNat (nat, 0) = nat
  | carryIntoNat (ZERO, c) = times10plus (ZERO, c)
  | carryIntoNat (TIMES10PLUS(natu, inte), 1) = times10plus
                                                (
                                                carryIntoNat 
                                                (natu, 
                                                (inte + 1) div 10),
                                                (inte + 1) mod 10
                                                )
   | carryIntoNat (nat, _) = raise notMachine



        val () = Unit.checkExpectWith Unit.stringString "carryIntoNat"
                (fn() => natString (carryIntoNat ( natOfInt 19,1)))
                "20" 

(*addWithCarry : nat * nat * int -> nat <adds two nats and a bit to return>*)

fun addWithCarry (nat, ZERO, c) = carryIntoNat(nat, c)
  | addWithCarry (ZERO, nat, c) = carryIntoNat(nat, c)
  | addWithCarry (TIMES10PLUS(natu, inte), TIMES10PLUS(natu1, inte1), c) =
                                let
                                    val d = (inte + inte1 + c) mod 10
                                    val c = (inte + inte1 + c) div 10
                            in times10plus (addWithCarry (natu, natu1, c), d)
                            end                       

        val () = Unit.checkExpectWith Unit.stringString "addWithCarry"
                (fn() => natString (addWithCarry (natOfInt 22 ,natOfInt 19,1)))
                "42" 

(*addNats : nat * nat -> nat <adds two nats to return>*)

fun addNats (nat, nat1) = addWithCarry(nat, nat1, 0)

        val () = Unit.checkExpectWith Unit.stringString "addNats"
                (fn() => natString (addNats (natOfInt 22 ,natOfInt 19)))
                "41" 

exception Negative

(*borrowFromNat : nat * int -> nat <subtracts bit from nat to return>*)

fun borrowFromNat (nat, 0) = nat
  | borrowFromNat (TIMES10PLUS(natu, 0), 1) = times10plus 
                                                (borrowFromNat (natu, 1), 9)
  | borrowFromNat (TIMES10PLUS(natu, inte), 1) = times10plus (natu, inte - 1)
  | borrowFromNat (nat, nat1) = if nat = ZERO andalso nat1 > 0 then 
                                    raise Negative else raise notMachine

        val () = Unit.checkExpectWith Unit.stringString "borrowFromNat"
                (fn() => natString (borrowFromNat( natOfInt 30,1)))
                "29" 

(*subWithBorrow : nat * nat * int -> nat <subtracts nat from another nat
                                          and bit to return>*)

fun subWithBorrow (nat, ZERO, b) = borrowFromNat(nat, b)
  | subWithBorrow (TIMES10PLUS(natu, inte), TIMES10PLUS(natu1, inte1), b)=
                        let val d = (inte - inte1 - b) mod 10
                            val b = if (inte - inte1 - b) < 0 then 1 else 0
                        in times10plus (subWithBorrow(natu, natu1, b), d)
                        end
  | subWithBorrow (ZERO, nat, b) = raise Negative

        val () = Unit.checkExpectWith Unit.stringString "subWithBorrow"
                (fn() => natString (subWithBorrow(natOfInt 30, natOfInt 0, 1)))
                "29" 

fun subNats (nat, nat1) = subWithBorrow (nat, nat1, 0)

        val () =
                  Unit.checkExnSatisfiesWith natString "1 - 5"
                  (fn () => subNats (natOfDigits [1], natOfDigits [5]))
                  ("Negative", fn Negative => true | _ => false)

(*subNats : nat * nat -> nat <subtracts nat from nat to return>*)

fun opsAgree name intop natop n1 n2 =
  Unit.checkExpectWith Int.toString name
  (fn () => intOfNat (natop (natOfInt n1, natOfInt n2)))
  (intop (n1, n2) handle Overflow => 0)



        val () = opsAgree "123 + 2018" (op +)  addNats 123 2018
        val () = opsAgree "2018 - 123" (op -)  subNats 2018 123
        val () = opsAgree "100 - 1   " (op -)  subNats 100 1


val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)



