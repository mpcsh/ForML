structure Annotate: sig

  datatype aterm
    = AUnit of Ty.ty
    | AVar of string * Ty.ty

    | ATrue of Ty.ty
    | AFalse of Ty.ty
    | AIf of aterm * aterm * aterm * Ty.ty

    | AZero of Ty.ty
    | ASucc of aterm * Ty.ty
    | APred of aterm * Ty.ty

    | ANil of Ty.ty
    | ACons of aterm * aterm * Ty.ty

    | APair of aterm * aterm * Ty.ty

    | ANone of Ty.ty
    | ASome of aterm * Ty.ty
    | ACase of aterm * string * Ty.ty * aterm * aterm * Ty.ty

    | AAbs of string * aterm * Ty.ty
    | AApp of aterm * aterm * Ty.ty

  val getTy: aterm -> Ty.ty Attempt.attempt
  val annotate: AST.term -> aterm Attempt.attempt

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

  datatype aterm
    = AUnit of Ty.ty
    | AVar of string * Ty.ty

    | ATrue of Ty.ty
    | AFalse of Ty.ty
    | AIf of aterm * aterm * aterm * Ty.ty

    | AZero of Ty.ty
    | ASucc of aterm * Ty.ty
    | APred of aterm * Ty.ty

    | ANil of Ty.ty
    | ACons of aterm * aterm * Ty.ty

    | APair of aterm * aterm * Ty.ty

    | ANone of Ty.ty
    | ASome of aterm * Ty.ty
    | ACase of aterm * string * Ty.ty * aterm * aterm * Ty.ty

    | AAbs of string * aterm * Ty.ty
    | AApp of aterm * aterm * Ty.ty

  fun getTy (AUnit ty) = pure ty
    | getTy (AVar (_, ty)) = pure ty

    | getTy (ATrue ty) = pure ty
    | getTy (AFalse ty) = pure ty
    | getTy (AIf (_, _, _, ty)) = pure ty

    | getTy (AZero ty) = pure ty
    | getTy (ASucc (_, ty)) = pure ty
    | getTy (APred (_, ty)) = pure ty

    | getTy (ANil ty) = pure ty
    | getTy (ACons (_, _, ty)) = pure ty

    | getTy (APair (_, _, ty)) = pure ty

    | getTy (ANone ty) = pure ty
    | getTy (ASome (_, ty)) = pure ty
    | getTy (ACase (_, _, _, _, _, ty)) = pure ty

    | getTy (AAbs (_, _, ty)) = pure ty
    | getTy (AApp (_, _, ty)) = pure ty

  fun annotate' g AST.Unit = pure (AUnit Ty.Unit)
    | annotate' g (AST.Var x) =
        TyEnv.lookup g x >>= (fn ty =>
        pure (AVar (x, ty)))

    | annotate' g AST.True = pure (ATrue Ty.Bool)
    | annotate' g AST.False = pure (AFalse Ty.Bool)
    | annotate' g (AST.If (e1, e2, e3)) =
        annotate' g e1 >>= (fn ae1 =>
        annotate' g e2 >>= (fn ae2 =>
        annotate' g e3 >>= (fn ae3 =>
        pure (AIf (ae1, ae2, ae3, Ty.freshTyVar ())))))

    | annotate' g AST.Zero = pure (AZero Ty.Int)
    | annotate' g (AST.Succ e) =
        annotate' g e >>= (fn ae =>
        pure (ASucc (ae, Ty.Int)))
    | annotate' g (AST.Pred e) =
        annotate' g e >>= (fn ae =>
        pure (APred (ae, Ty.Int)))

    | annotate' g AST.Nil = pure (ANil (Ty.List (Ty.freshTyVar ())))
    | annotate' g (AST.Cons (e1, e2)) =
        annotate' g e1 >>= (fn ae1 =>
        annotate' g e2 >>= (fn ae2 =>
        pure (ACons (ae1, ae2, Ty.List (Ty.freshTyVar ())))))

    | annotate' g (AST.Pair (e1, e2)) =
        annotate' g e1 >>= (fn ae1 =>
        annotate' g e2 >>= (fn ae2 =>
        pure (APair (ae1, ae2, Ty.Pair (Ty.freshTyVar (), Ty.freshTyVar ())))))

    | annotate' g AST.None = pure (ANone (Ty.Option (Ty.freshTyVar ())))
    | annotate' g (AST.Some e) =
        annotate' g e >>= (fn ae =>
        pure (ASome (ae, Ty.Option (Ty.freshTyVar ()))))
    | annotate' g (AST.Case (e1, x, e2, e3)) =
      let
        val xTy = Ty.freshTyVar ()
      in
        annotate' g e1                >>= (fn ae1 =>
        annotate' (g <+> (x, xTy)) e2 >>= (fn ae2 =>
        annotate' g e3                >>= (fn ae3 =>
        pure (ACase (ae1, x, xTy, ae2, ae3, Ty.freshTyVar ())))))
      end

    | annotate' g (AST.Abs (arg, e)) =
      let
        val argTy = Ty.freshTyVar ()
      in
        annotate' (g <+> (arg, argTy)) e >>= (fn ae =>
        getTy ae                         >>= (fn retTy =>
        pure (AAbs (arg, ae, Ty.Fun (argTy, retTy)))))
      end
    | annotate' g (AST.App (f, arg)) =
        annotate' g f   >>= (fn af =>
        annotate' g arg >>= (fn aarg =>
        pure (AApp (af, aarg, Ty.freshTyVar ()))))

    (* | annotate' g (AST.Let (x, NONE, e', e)) = *)
    (*     annotate' g (AST.Let (x, SOME (Ty.freshTyVar ()), e', e)) *)
    (* | annotate' g (AST.Let (x, SOME ty, e', e)) = *)
    (*     annotate' (g <+> (x, ty)) e' >>= (fn ae' => *)
    (*     annotate' (g <+> (x, ty)) e  >>= (fn ae => *)
    (*     pure (ALet (x, ty, ae', ae, Ty.freshTyVar ())))) *)

    (* TODO: what do we do with type annotations? *)
    | annotate' g (AST.Let (_, _, _, _)) =
        Failure "annotate: unexpected let (should've been reduced away)"

  val annotate = annotate' []

end
