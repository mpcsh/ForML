structure TyEnv: sig

  type tyenv = (string * Ty.ty) list

  val empty: tyenv
  val lookup: tyenv -> string -> Ty.ty Attempt.attempt
  val <+> : tyenv * (string * Ty.ty) -> tyenv

end = struct

  open Attempt

  open AttemptMonad
  structure AttemptMonadUtil = MonadUtil(AttemptMonad)
  open AttemptMonadUtil
  infixr 0 $
  infix 1 >>= >>
  infixr 1 =<< >=> <=<
  infix 4 <$> <*>

  open Ty

  infix \/
  fun f1 \/ f2 = fn x =>
    case (f1 x)
      of Success x' => Success x'
       | Failure _ => (f2 x)

  type tyenv = (string * Ty.ty) list

  val empty = []

  fun basis "succ" = Success (Fun (Int, Int))
    | basis "pred" = Success (Fun (Int, Int))
    | basis "zero?" = Success (Fun (Int, Bool))
    | basis "cons" = let val alpha = freshTyVar () in
        Success (Fun (alpha, Fun (List alpha, List alpha)))
      end
    | basis "hd" = let val alpha = freshTyVar () in
        Success (Fun (List alpha, alpha))
      end
    | basis "tl" = let val alpha = freshTyVar () in
        Success (Fun (List alpha, List alpha))
      end
    | basis "nil?" = let val alpha = freshTyVar () in
        Success (Fun (List alpha, Bool))
      end
    | basis "fst" = let val alpha = freshTyVar () val beta = freshTyVar () in
        Success (Fun (Pair (alpha, beta), alpha))
      end
    | basis "snd" = let val alpha = freshTyVar () val beta = freshTyVar () in
        Success (Fun (Pair (alpha, beta), beta))
      end
    | basis "plus" = Success (Fun (Int, Fun (Int, Int)))
    | basis "minus" = Success (Fun (Int, Fun (Int, Int)))
    | basis "times" = Success (Fun (Int, Fun (Int, Int)))
    | basis "and" = Success (Fun (Bool, Fun (Bool, Bool)))
    | basis "or" = Success (Fun (Bool, Fun (Bool, Bool)))
    | basis "not" = Success (Fun (Bool, Bool))
    | basis f = Failure ("no such basis function: " ^ f)

  fun binding [] x = Failure ("identifier " ^ x ^ " not bound")
    | binding ((y, ty) :: gs) x =
        if x = y
        then Success ty
        else binding gs x

  fun lookup env x = (basis \/ (binding env)) x

  infix <+>
  fun g <+> (x, ty) = (x, ty) :: g

end
