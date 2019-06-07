structure AST: sig

  datatype term
    = Unit
    | Var of string

    | True
    | False
    | If of term * term * term

    | Zero
    | Succ of term
    | Pred of term

    | Nil
    | Cons of term * term

    | Pair of term * term

    | None
    | Some of term
    | Case of term * string * term * term

    | Abs of string * term
    | App of term * term

    | Let of string * Ty.ty option * term * term

  val subst: term -> string -> term -> term

  val isValue: term -> bool

  val unparse: term -> string

end = struct

  open Attempt

  open AttemptMonad
  structure AttemptMonadUtil = MonadUtil(AttemptMonad)
  open AttemptMonadUtil
  infixr 0 $
  infix 1 >>= >>
  infixr 1 =<< >=> <=<
  infix 4 <$> <*>


  datatype term
    = Unit
    | Var of string

    | True
    | False
    | If of term * term * term

    | Zero
    | Succ of term
    | Pred of term

    | Nil
    | Cons of term * term

    | Pair of term * term

    | None
    | Some of term
    | Case of term * string * term * term

    | Abs of string * term
    | App of term * term

    | Let of string * Ty.ty option * term * term


  (* substitute s for x in e *)
  fun subst _ _ Unit = Unit
    | subst s x (Var y) =
        if x = y
        then s
        else Var y
    | subst _ _ True = True
    | subst _ _ False = False
    | subst s x (If (e1, e2, e3)) = crank3 s x e1 e2 e3 If
    | subst _ _ Zero = Zero
    | subst s x (Succ e) = crank1 s x e Succ
    | subst s x (Pred e) = crank1 s x e Pred
    | subst _ _ Nil = Nil
    | subst s x (Cons (e1, e2)) = crank2 s x e1 e2 Cons
    | subst s x (Pair (e1, e2)) = crank2 s x e1 e2 Pair
    | subst _ _ None = None
    | subst s x (Some e) = crank1 s x e Some
    | subst s x (Case (e1, x2, e2, e3)) =
      let
        val (x2', e2Pre) = freshen x x2 e2
      in
        crank3 s x e1 e2Pre e3 (fn (e1', e2', e3') => (Case (e1', x2', e2', e3')))
      end
    | subst s x (Abs (y, e)) =
      let
        val (y', ePre) = freshen x y e
      in
        crank1 s x ePre (fn e' => Abs (y', e'))
      end
    | subst s x (App (e1, e2)) = crank2 s x e1 e2 App
    | subst s x (Let (y, ty, e1, e2)) =
      let
        val (y', e1Pre) = freshen x y e1
        val (y', e2Pre) = freshen x y e2
      in
        crank2 s x e1Pre e2Pre (fn (e1', e2') => Let (y', ty, e1', e2'))
      end

  and freshen x1 x2 e =
    if x1 = x2
    then ("@" ^ x2, subst (Var ("@" ^ x2)) x2 e)
    else (x2, e)

  and crank1 s x e con = con (subst s x e)
  and crank2 s x e1 e2 con =
    let
      val e1' = subst s x e1
      val e2' = subst s x e2
    in
      con (e1', e2')
    end
  and crank3 s x e1 e2 e3 con =
    let
      val e1' = subst s x e1
      val e2' = subst s x e2
      val e3' = subst s x e3
    in
      con (e1', e2', e3')
    end


  fun isValue Unit = true

    | isValue (True | False) = true

    | isValue Zero = true
    | isValue (Succ e) = isValue e
    | isValue (Pred _) = false

    | isValue Nil = true
    | isValue (Cons (e1, e2)) = isValue e1 andalso isValue e2

    | isValue (Pair (e1, e2)) = isValue e1 andalso isValue e2

    | isValue None = true
    | isValue (Some e) = isValue e

    | isValue (Abs (_, _)) = true

    | isValue _ = false


  fun fromPeano Zero = 0
    | fromPeano (Succ e) = (fromPeano e) + 1
    | fromPeano (Pred e) = (fromPeano e) - 1
    | fromPeano _ = raise Fail "cannot convert non-numeric term"

  local open Utils in
  fun unparse Unit = "()"
    | unparse (Var x) = x

    | unparse True = "true"
    | unparse False = "false"
    | unparse (If (e1, e2, e3)) =
        spc ["if", (unparse e1), "then", (unparse e2), "else", (unparse e3)]

    | unparse (e as (Zero | (Succ _) | (Pred _))) = (Int.toString o fromPeano) e

    | unparse Nil = "[]"
    | unparse (Cons (e1, e2)) = spc [unparse e1, "::", unparse e2]

    | unparse (Pair (e1, e2)) = paren ((cat o between [unparse e1, unparse e2]) ", ")

    | unparse None = "None"
    | unparse (Some e) = spc ["Some", unparse e]
    | unparse (Case (e1, x, e2, e3)) = spc [
        "case", unparse e1,
        "of", spc ["Some", x], "=>", unparse e2,
        "|", "None", "=>", unparse e3
      ]

    | unparse (Abs (x, e)) =
        spc ["fn", x, "=>", unparse e]
    | unparse (App ((Var f), arg)) = spc [f, unparse arg]
    | unparse (App (e, arg)) = spc [(paren o unparse) e, unparse arg]

    | unparse (Let (x, (SOME ty), e', e)) =
        spc ["let", x, ":", Ty.unty ty, "=", unparse e', "in", unparse e]
    | unparse (Let (x, NONE, e', e)) =
        spc ["let", x, "=", unparse e', "in", unparse e]
  end

end
