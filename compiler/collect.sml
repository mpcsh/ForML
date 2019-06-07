structure Collect: sig

  val collect: Annotate.aterm -> (Ty.ty * Ty.ty) list Attempt.attempt

end = struct

  open Attempt

  open AttemptMonad
  structure AttemptMonadUtil = MonadUtil(AttemptMonad)
  open AttemptMonadUtil
  infixr 0 $
  infix 1 >>= >>
  infixr 1 =<< >=> <=<
  infix 4 <$> <*>

  val <+> = TyEnv.<+>
  infix <+>

  local open Ty in local open Annotate in
  fun collect' [] cs = pure cs
    | collect' (AIf (ae1, ae2, ae3, ty) :: aes) cs =
        getTy ae1 >>= (fn ae1Ty =>
        getTy ae2 >>= (fn ae2Ty =>
        getTy ae3 >>= (fn ae3Ty =>
        collect' (ae1 :: ae2 :: ae3 :: aes)
                 ((ae1Ty, Bool) :: (ae2Ty, ae3Ty) :: (ae2Ty, ty) :: cs))))

    | collect' (ACons (hd, tl, List ty) :: aes) cs =
        getTy hd >>= (fn hdTy =>
        getTy tl >>= (fn tlTy =>
        collect' (hd :: tl :: aes) ((hdTy, ty) :: (tlTy, List ty) :: cs)))
    | collect' (ACons (_, _, _) :: _) _ =
        Failure "received an annotated cons that didn't have a list type"

    | collect' (APair (ae1, ae2, ty) :: aes) cs =
        getTy ae1 >>= (fn ae1Ty =>
        getTy ae2 >>= (fn ae2Ty =>
        collect' (ae1 :: ae2 :: aes) ((ty, Pair (ae1Ty, ae2Ty)) :: cs)))

    | collect' (ASome (ae, ty) :: aes) cs =
        getTy ae >>= (fn aeTy =>
        collect' (ae :: aes) ((ty, Option aeTy) :: cs))

    | collect' (ACase (ae1, _, ty2, ae2, ae3, ty) :: aes) cs =
        getTy ae1 >>= (fn ae1Ty =>
        getTy ae2 >>= (fn ae2Ty =>
        getTy ae3 >>= (fn ae3Ty =>
        collect' (ae1 :: ae2 :: ae3 :: aes)
          ((ae1Ty, Option ty2) :: (ae2Ty, ae3Ty) :: (ae2Ty, ty) :: cs))))

    | collect' (AAbs (_, ae, _) :: aes) cs = collect' (ae :: aes) cs
    | collect' (AApp (f, arg, ty) :: aes) cs =
        getTy f   >>= (fn fTy =>
        getTy arg >>= (fn argTy =>
        collect' (f :: arg :: aes) ((fTy, Fun (argTy, ty)) :: cs)))

    (* | collect' (ALet (x, ty', ae', ae, ty) :: aes) cs = *)
    (*     getTy ae' >>= (fn ae'Ty => *)
    (*     getTy ae  >>= (fn aeTy => *)
    (*     collect' (ae' :: ae :: aes) ((ae'Ty, ty') :: (aeTy, ty) :: cs))) *)

    | collect' (_ :: aes) cs = collect' aes cs
  end end

  fun collect ae = collect' [ae] []

end
