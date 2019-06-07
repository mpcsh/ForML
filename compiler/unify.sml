structure Unify: sig

  type substitution = (string * Ty.ty) list

  val occurs: string -> Ty.ty -> bool
  val subst: Ty.ty -> string -> Ty.ty -> Ty.ty
  val unify: (Ty.ty * Ty.ty) list -> substitution Attempt.attempt
  val apply: substitution -> Ty.ty -> Ty.ty

end = struct

  open Attempt

  open AttemptMonad
  structure AttemptMonadUtil = MonadUtil(AttemptMonad)
  open AttemptMonadUtil
  infixr 0 $
  infix 1 >>= >>
  infixr 1 =<< >=> <=<
  infix 4 <$> <*>

  type substitution = (string * Ty.ty) list

  open Ty

  fun occurs _ (Unit | Bool | Int) = false
    | occurs x (List ty) = occurs x ty
    | occurs x (Pair (ty1, ty2)) = occurs x ty1 orelse occurs x ty2
    | occurs x (Option ty) = occurs x ty
    | occurs x (Fun (arg, ret)) = occurs x arg orelse occurs x ret
    | occurs x (TyVar y) = x = y

  (* subst s for x in t *)
  fun subst _ _ (x as (Unit | Bool | Int)) = x
    | subst s x (List ty) = List (subst s x ty)
    | subst s x (Pair (ty1, ty2)) = Pair (subst s x ty1, subst s x ty2)
    | subst s x (Option ty) = Option (subst s x ty)
    | subst s x (Fun (arg, ret)) = Fun (subst s x arg, subst s x ret)
    | subst s x (TyVar y) =
        if x = y
        then s
        else TyVar y

  fun apply s t = foldr (fn ((x, t'), e) => subst t' x e) t s

  fun unifyIfNotOccurs x ty =
    if occurs x ty
    then Failure ("Cannot unify " ^ x ^ " with " ^ (unty ty))
    else Success [(x, ty)]

  fun unify' Unit Unit = pure []
    | unify' Unit (TyVar x) = pure [(x, Unit)]
    | unify' (TyVar x) Unit = pure [(x, Unit)]

    | unify' Bool Bool = pure []
    | unify' Bool (TyVar x) = pure [(x, Bool)]
    | unify' (TyVar x) Bool = pure [(x, Bool)]

    | unify' Int Int = pure []
    | unify' Int (TyVar x) = pure [(x, Int)]
    | unify' (TyVar x) Int = pure [(x, Int)]

    | unify' (List ty1) (List ty2) = unify' ty1 ty2

    | unify' (Pair (ty1, ty2)) (Pair (ty1', ty2')) =
        unify [(ty1, ty1'), (ty2, ty2')]

    | unify' (Option ty1) (Option ty2) = unify' ty1 ty2

    | unify' (Fun (arg1, ret1)) (Fun (arg2, ret2)) =
        unify [(arg1, arg2), (ret1, ret2)]

    | unify' (TyVar x) (TyVar y) =
        if x = y
        then pure []
        else pure [(y, TyVar x)]
    | unify' ty (TyVar x) = unifyIfNotOccurs x ty
    | unify' (TyVar x) ty = unifyIfNotOccurs x ty

    | unify' x y = Failure ("cannot unify " ^ (unty x) ^ " with " ^ (unty y))

  and unify [] = pure []
    | unify ((x, y) :: t) =
        unify t                          >>= (fn t2 =>
        unify' (apply t2 x) (apply t2 y) >>= (fn t1 =>
        pure (t1 @ t2)))

end
