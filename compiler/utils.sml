structure Utils: sig

  (* print the string, then a newline *)
  val println: string -> unit

  (* take items from list as long as items pass the test *)
  val takeWhile: ('a -> bool) -> 'a list -> 'a list * 'a list

  (* split a list at a given length, returning both sides of the split *)
  val splitAt: 'a list -> int -> ('a list * 'a list)

  (* take as many as n items from the list *)
  val upto: int -> 'a list -> 'a list

  val err: string -> string -> 'a

  val andmap: ('a -> bool) -> 'a list -> bool
  val ormap: ('a -> bool) -> 'a list -> bool
  val zipEq: 'a list -> 'b list -> ('a * 'b) list

  (* un-X helpers *)
  val spc: string list -> string
  val cat: string list -> string
  val paren: string -> string
  val brc: string -> string
  val between: string list -> string -> string list

end = struct

  fun println s = (TextIO.print s; TextIO.print "\n")

  fun takeWhile f xs =
  let
    fun lp ([], acc) = (rev acc, [])
      | lp (list as x::xs, acc) =
          if f x
          then lp (xs, x::acc)
          else (rev acc, list)
  in
    lp (xs, [])
  end

  fun upto _ [] = []
    | upto n (x::xs) =
        if n <= 0
        then []
        else x::(upto (n - 1) xs)

  fun splitAt [] _ = ([], [])
    | splitAt xs 0 = ([], xs)
    | splitAt (x :: xs) n =
      let
        val (l, r) = splitAt xs (n - 1)
      in
        (x :: l, r)
      end

  fun err kind msg =
    raise Fail (kind ^ " error: " ^ msg)

  fun spc [] = ""
    | spc [t] = t
    | spc (t :: ts) = t ^ " " ^ (spc ts)

  fun cat [] = ""
    | cat (t :: ts) = t ^ (cat ts)

  fun paren x = "(" ^ x ^ ")"

  fun brc x = "[" ^ x ^ "]"

  fun between [] _ = []
    | between [x] _ = [x]
    | between (x :: xs) b = [x, b] @ (between xs b)

  fun andmap _ [] = true
    | andmap f (x :: xs) = (f x) andalso (andmap f xs)

  fun ormap _ [] = false
    | ormap f (x :: xs) = (f x) orelse (ormap f xs)

  fun zipEq [] [] = []
    | zipEq _ [] = raise Fail "cannot zipEq lists of different lengths"
    | zipEq [] _ = raise Fail "cannot zipEq lists of different lengths"
    | zipEq (x :: xs) (y :: ys) = (x, y) :: zipEq xs ys

end
