(***** COMP 105 ML ASSIGNMENT *****)

(***** Problem 1 *****)

(* Function mynull is of 'a list -> bool, it takes a list and return a *)
(* boolean indicating whether it's empty or not                        *)

fun mynull []      = true
  | mynull (y::ys) = false


(* Unit Test *)
        val () =
            Unit.checkAssert "[] is null"
            (fn () => mynull [])

        val () =
            Unit.checkExpectWith Unit.boolString "[] is not null"
            (fn () => mynull [1, 2, 3])
            false

(***** Problem 2 a *****)

(* Function reverse is of 'a list -> 'a list, it takes a list and return *)
(* a list of its elements in reverse order                               *)

fun reverse []      = []
  | reverse (x::xs) = foldl (op ::) [] (x::xs)


(* (int list) string buffer for unit testing *)
val int_list_toString = Unit.listString Unit.intString

(* Unit Test *)
        val () =
            Unit.checkExpectWith int_list_toString "reversing empty"
            (fn () => reverse [])
            []

        val () =
            Unit.checkExpectWith int_list_toString "reversing list"
            (fn () => reverse [1, 2, 3, 4])
            [4, 3, 2, 1]

(***** Problem 2 b *****)

exception NoMatch

(* Function minlist is of int list -> int, it takes a nonempty integer list *)
(* and return its smallest element. It will raise exception if applied to   *)
(* an empty list                                                            *)

fun minlist []      = raise NoMatch
  | minlist (x::xs) = foldr Int.min x (x::xs)


(* Unit Test *)
        val () =
            Unit.checkExpectWith Unit.intString "smallest of singleton"
            (fn () => minlist [1])
            1

        val () =
            Unit.checkExpectWith Unit.intString "smallest of list"
            (fn () => minlist [1, ~1, 89, ~90])
            ~90

        val () =
            Unit.checkExnWith Unit.intString "smallest of empty_list"
            (fn () => minlist [])

(***** Problem 3 *****)

exception Mismatch

(* Function zip is of 'a list * 'b list -> ('a * 'b) list, it takes a       *)
(* pair of equal-length lists and returns the equivalent list of pairs. If  *)
(* the lengths don’t match, it will raise the exception Mismatch            *)

fun zip ((x::xs), (y::ys)) = (x, y) :: zip (xs, ys)
  | zip ([], [])           = []
  | zip _                  = raise Mismatch


(* Some new string builders *)
val int_pair_toString  = Unit.pairString Unit.intString Unit.intString
val list_pair_toString = Unit.listString int_pair_toString
(* Unit Test *)
        val () =
            Unit.checkExpectWith list_pair_toString "zip on integer lists"
            (fn () => zip ([1, ~2, 4], [9, 12, 0]))
            [(1, 9), (~2, 12), (4, 0)]

        val () =
            Unit.checkExpectWith list_pair_toString "zip on empty lists"
            (fn () => zip ([], []))
            []

        val () =
            Unit.checkExnWith list_pair_toString "zip on integer lists"
            (fn () => zip ([1, 2], [9]))

(***** Problem 4 *****)

(* Function pairfoldrEq is ('a * 'b * 'c -> 'c)->'c -> 'a list * 'b list ->'c *)
(* it applies a three-argument function to a pair of lists of equal length,   *)
(* it will raise exception Mismatch if lists' lengths don't match             *)

fun pairfoldrEq p zero ([], [])           = zero
  | pairfoldrEq p zero ((x::xs), (y::ys)) = p (x, y, pairfoldrEq p zero (xs,ys))
  | pairfoldrEq p zero _                  = raise Mismatch

(* Function ziptoo is of 'a list * 'b list -> ('a * 'b) list, it does the  *)
(* same thing as zip.                                                      *)

fun ziptoo xs =
    let fun f (i, j, cdr) = (i, j) :: cdr
    in  pairfoldrEq f [] xs
    end

(* Unit Test *)

        val () =
            Unit.checkExpectWith Unit.intString "pairfoldrEq on integer lists"
            (fn () =>
                pairfoldrEq (fn (x, y, cdr) => x + y + cdr) 0
                            ([1, ~2, 4], [9, 12, 0]))
            24

        val () =
            Unit.checkExnWith Unit.intString
                              "pairfoldrEq on lists of unequal length"
            (fn () =>
                pairfoldrEq (fn (x, y, cdr) => x + y + cdr) 0
                            ([1, ~2], [9, 12, 0]))

        val () =
            Unit.checkExpectWith Unit.intString
                                 "pairfoldrEq on pair of empty lists"
            (fn () =>
                pairfoldrEq (fn (x, y, cdr) => x + y + cdr) 0 ([], []))
            0

        val () =
            Unit.checkExpectWith list_pair_toString "zip on integer lists"
            (fn () => ziptoo ([1, ~2, 4], [9, 12, 0]))
            [(1, 9), (~2, 12), (4, 0)]

        val () =
            Unit.checkExpectWith list_pair_toString "zip on empty lists"
            (fn () => ziptoo ([], []))
            []

        val () =
            Unit.checkExnWith list_pair_toString
                              "zip on lists of unequal length"
            (fn () => ziptoo ([1, 2], [9]))

(***** Problem 5 *****)

(* Function concat is of 'a list list -> 'a list, it takes a list of lists of *)
(* 'a and produces a single list of 'a containing all the elements in the     *)
(* correct order.                                                             *)

fun concat (x::xs) = foldr (op @) [] (x::xs)
  | concat []      = []

(* Unit Test *)

        val () =
            Unit.checkExpectWith int_list_toString
                                 "concat on a list of empty lists"
            (fn () => concat [[], [], []])
            []
        val () =
            Unit.checkExpectWith int_list_toString
                                 "concat on a list of integer lists"
            (fn () => concat [[1, 3], [2], [], [9, 10]])
            [1, 3, 2, 9, 10]

        val bool_list_toString = Unit.listString Unit.boolString
        val () =
            Unit.checkExpectWith bool_list_toString
                                 "concat on a list of boolean lists"
            (fn () => concat [[true, false], [true], [], [false, false]])
            [true, false, true, false, false]

(***** Problem 6 a *****)

(* definition of ordinary S-expressions *)
datatype ordsx = BOOL of bool
               | NUM  of int
               | SYM  of string
               | SXS  of ordsx list

(* Function numbersSx is of int list -> ordsx, it takes a list of numbers, *)
(* and returns its ordinary S-expression by definition above               *)

fun numbersSx (x::xs) =
    let fun f (x, xs) = NUM x :: xs
    in  SXS   (foldr f [] (x::xs))
    end
  | numbersSx []      = SXS []


(* Unit Test *)
(* Function sxString is defined for unit testing, it takes a ordsx type    *)
(* and returns its string representation                                   *)
fun sxString (SYM s)   = s
  | sxString (NUM n)   = Unit.intString n
  | sxString (BOOL b)  = if b then "true" else "false"
  | sxString (SXS sxs) = "(" ^ String.concatWith " " (map sxString sxs) ^ ")"

        val () =
            Unit.checkExpectWith Unit.stringString
                                 "numbersSx on list of numbers"
            (fn () => sxString (numbersSx [1, 9, 180, ~1]))
            "(1 9 180 ~1)"

        val () =
            Unit.checkExpectWith Unit.stringString "numbersSx on empty list"
            (fn () => sxString (numbersSx []))
            "()"

(***** Problem 6 b *****)

(* Function flattenSyms is of ordsx -> string list, it takes an ordsx and    *)
(* retrieves only the symbols from it, it returns a list of strings of these *)
(* symbols.                                                                  *)

fun flattenSyms (SYM s)                    = [s]
  | flattenSyms (SXS sxs)                  =
        let fun symbol      (SYM s)        = true
              | symbol      _              = false
            fun toString    ((SYM s), sxs) = s::sxs
              | toString    _              = []
        in  foldr toString [] (List.filter symbol sxs)
        end
  | flattenSyms _                          = []

(* Unit Test *)
        val string_list_toString = Unit.listString Unit.stringString
        val () =
            Unit.checkExpectWith string_list_toString "flatten on symbol"
            (fn () => flattenSyms (SYM "apple"))
            ["apple"]

        val () =
            Unit.checkExpectWith string_list_toString "flatten on ordsx list"
            (fn () => flattenSyms
                      (SXS [SYM "apple", NUM 1, BOOL true, SYM "cat", NUM 10]))
            ["apple", "cat"]

        val () =
            Unit.checkExpectWith string_list_toString "flatten on ordsx list"
            (fn () => flattenSyms
                      (SXS [SYM "apple", SXS [SYM "dog"], BOOL true, NUM 10]))
            ["apple"]

        val () =
            Unit.checkExpectWith string_list_toString "flatten on number"
            (fn () => flattenSyms (BOOL true))
            []

val () = Unit.reportWhenFailures ()
