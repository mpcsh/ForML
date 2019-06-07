structure TyScheme: sig

  type tyScheme = {sigmas: string list, tau: Ty.ty}

  val generalize: Ty.ty -> tyScheme

  val instantiate: tyScheme -> Ty.ty

  val untyScheme: tyScheme -> string

end = struct

  type tyScheme = {sigmas: string list, tau: Ty.ty}

  local open Ty in
  fun freeVars' (Unit | Bool | Int) = []
    | freeVars' (List ty) = freeVars' ty
    | freeVars' (Pair (ty1, ty2)) = (freeVars' ty1) @ (freeVars' ty2)
    | freeVars' (Option ty) = freeVars' ty
    | freeVars' (Fun (ty1, ty2)) = (freeVars' ty1) @ (freeVars' ty2)
    | freeVars' (TyVar a) = [a]
  end
  fun dedup [] = []
    | dedup (x :: xs) =
        if List.exists (fn x' => x = x') xs
        then dedup xs
        else x :: dedup xs
  val freeVars = dedup o freeVars'

  fun generalize ty =
    {sigmas = freeVars ty, tau = ty}

  fun instantiate {sigmas = sigmas, tau = ty} =
    foldl (fn (a, inner) => Unify.subst (Ty.freshTyVar ()) a inner) ty sigmas

  local open Utils in
  fun untyScheme {sigmas = sigmas, tau = ty} =
    cat (
      (map (fn a => cat ["\\/", a, "."]) sigmas) @
      [Ty.unty ty]
    )
  end

end
