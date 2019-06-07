structure Ty: sig

  datatype ty
    = Unit
    | Bool
    | Int
    | List of ty
    | Pair of ty * ty
    | Option of ty
    | Fun of ty * ty
    | TyVar of string

  val unty: ty -> string

  val resetTyVars: unit -> unit
  val freshTyVar: unit -> ty
  val freshPolishedTyVar: unit -> ty

end = struct

  (* type declarations *)
  datatype ty
    = Unit
    | Bool
    | Int
    | List of ty
    | Pair of ty * ty
    | Option of ty
    | Fun of ty * ty
    | TyVar of string

  local open Utils in
  fun nest (ty as (Pair (ty1, ty2) | Fun (ty1, ty2))) =
        paren (unty ty)
    | nest ty = unty ty

  and unty Unit = "unit"
    | unty Bool = "bool"
    | unty Int = "int"
    | unty (List ty) = spc [(nest ty), "list"]
    | unty (Pair (ty1, ty2)) = (spc o between (map nest [ty1, ty2])) "*"
    | unty (Option ty) = spc [(nest ty), "option"]
    | unty (Fun (arg, res)) = (spc o between (map nest [arg, res])) "->"
    | unty (TyVar v) = v
  end


  (* type variable utilities *)
  val counter = ref 1
  val polishedCounter = ref [0]

  fun incrementPolished ns =
    if List.all (fn n => n = 25) ns
    then List.tabulate ((length ns) + 1, (fn _ => 0))
    else let
      val (rsuffix, rprefix) = Utils.takeWhile (fn n => n = 25) (rev ns)
      val newPrefix = rev (((hd rprefix) + 1) :: (tl rprefix))
    in
      newPrefix @ (rev rsuffix)
    end

  fun resetTyVars () =
    (counter := 0; polishedCounter := [0])

  fun freshTyVar () =
  let
    val v = "@" ^ (Int.toString (!counter))
    val _ = counter := (!counter) + 1
  in
    TyVar v
  end

  fun freshPolishedTyVar () =
  let
    val v = implode (#"'" :: (map (fn n => Char.chr (n + (Char.ord #"a"))) (!polishedCounter)))
    val _ = polishedCounter := incrementPolished (!polishedCounter)
  in
    TyVar v
  end

end
