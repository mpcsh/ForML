structure Polish: sig

  val polish: Ty.ty -> Ty.ty

end = struct

  open Ty

  fun lookupOrPolish [] x = (x, freshPolishedTyVar ())
    | lookupOrPolish ((x': string, tv': ty) :: ps) x =
        if x = x'
        then (x, tv')
        else lookupOrPolish ps x

  fun polish' ps (Unit | Bool | Int) = ps
    | polish' ps (List ty) = polish' ps ty
    | polish' ps (Option ty) = polish' ps ty
    | polish' ps (Pair (ty1, ty2)) =
        foldl (fn (ty', ps') => polish' ps' ty') ps [ty1, ty2]
    | polish' ps (Fun (arg, ret)) =
        foldl (fn (ty', ps') => polish' ps' ty') ps [arg, ret]
    | polish' ps (TyVar x) =
        ps @ [lookupOrPolish ps x]

  fun polish ty = Unify.apply (polish' [] ty) ty

end
