(* structure Eval: sig *)

(*   val eval: AST.term -> AST.term Attempt.attempt *)

(* end = struct *)
structure Eval = struct

  open OptAttempt

  open OptAttemptMonad
  structure OptAttemptMonadUtil = MonadUtil(OptAttemptMonad)
  open OptAttemptMonadUtil
  infixr 0 $
  infix 1 >>= >>
  infixr 1 =<< >=> <=<
  infix 4 <$> <*>


  local open AST in
  fun subst _ _ (t as ((Int _) | True | False | (String _) | Unit | (List []))) =
        Success t

    | subst x v (List (t :: ts)) =
        subst x v t >>= (fn t' =>
        forM ts (subst x v) >>= (fn ts' =>
        Success (List (t' :: ts'))))
    | subst x v (Pair (t1, t2)) =
        subst x v t1 >>= (fn t1' =>
        subst x v t2 >>= (fn t2' =>
        Success (Pair (t1', t2'))))
    | subst x v (Left t) =
        subst x v t >>= (fn t' =>
        Success (Left t'))
    | subst x v (Right t) =
        subst x v t >>= (fn t' =>
        Success (Right t'))

    | subst x v (If (t1, t2, t3)) =
        subst x v t1 >>= (fn t1' =>
        subst x v t2 >>= (fn t2' =>
        subst x v t3 >>= (fn t3' =>
        Success (If (t1', t2', t3')))))
    | subst x v (Case (t1, x2, t2, x3, t3)) =
        subst x v t1 >>= (fn t1' =>
          if x = x2 andalso x = x3
          then Success (Case (t1', x2, t2, x3, t3))
          else if x = x2
          then
            subst x v t3 >>= (fn t3' =>
            Success (Case (t1', x2, t2, x3, t3')))
          else if x = x3
          then
            subst x v t2 >>= (fn t2' =>
            Success (Case (t1', x2, t2', x3, t3)))
          else
            subst x v t2 >>= (fn t2' =>
            subst x v t3 >>= (fn t3' =>
            Success (Case (t1', x2, t2', x3, t3')))))
    | subst x v (t as Fun (x', t')) =
        if x = x'
        then Success t
        else
          subst x v t' >>= (fn t'' =>
          Success (Fun (x', t'')))
    | subst x v (App (t1, t2)) =
        subst x v t1 >>= (fn t1' =>
        subst x v t2 >>= (fn t2' =>
        Success (App (t1', t2'))))
    | subst x v (t as Var x') =
        if x = x'
        then Success v
        else Success t
  end


  local open AST in
  fun basis "succ" [Int n] = pure (Int (n + 1))
    | basis "pred" [Int n] = pure (Int (n - 1))
    | basis "zero?" [Int 0] = pure True
    | basis "zero?" [Int _] = pure False
    | basis "cons" [x, List xs] = pure (List (x :: xs))
    | basis "hd" [List []] = Failure "cannot take head of empty list!"
    | basis "hd" [List (x :: _)] = pure x
    | basis "tl" [List []] = Failure "cannot take tail of empty list!"
    | basis "tl" [List (_ :: xs)] = pure (List xs)
    | basis "nil?" [List []] = pure True
    | basis "nil?" [List _] = pure False
    | basis "fst" [Pair (x, _)] = pure x
    | basis "snd" [Pair (_, x)] = pure x
    | basis "plus" [Int x, Int y] = pure (Int (x + y))
    | basis "minus" [Int x, Int y] = pure (Int (x - y))
    | basis "times" [Int x, Int y] = pure (Int (x * y))
    | basis "and" [False, _] = pure False
    | basis "and" [True, t] = pure t
    | basis "or" [True, _] = pure True
    | basis "or" [False, t] = pure t
    | basis "not" [True] = pure False
    | basis "not" [False] = pure True
    | basis f xs =
        if Utils.andmap isValue xs
        then
          Failure (
            "either no such basis function " ^ f ^
            ", or cannot apply " ^ f ^ " to args " ^ (Utils.spc (map unparse xs)))
        else
          forM xs step >>= (fn xs' =>
          basis f xs')

  and step (t as ((Int _) | True | False | (String _) | Unit | (List []))) =
        Terminal t

    | step (List (t :: ts)) =
        if Utils.andmap isValue (t :: ts)
        then Terminal (List (t :: ts))
        else
          step t       >>= (fn t' =>
          forM ts step >>= (fn ts' =>
          pure (List (t' :: ts'))))
    | step (Pair (t1, t2)) =
        if isValue t1 andalso isValue t2
        then Terminal (Pair (t1, t2))
        else
          step t1 >>= (fn t1' =>
          step t2 >>= (fn t2' =>
          pure (Pair (t1', t2'))))
    | step (Left t) =
        if isValue t
        then Terminal (Left t)
        else
          step t >>= (fn t' =>
          pure (Left t'))
    | step (Right t) =
        if isValue t
        then Terminal (Right t)
        else
          step t >>= (fn t' =>
          pure (Right t'))

    | step (If (t1, t2, t3)) =
        if isValue t1
        then
          if t1 = True
          then pure t2
          else pure t3
        else
          step t1 >>= (fn t1' =>
          pure (If (t1', t2, t3)))
    | step (Case (Left t1, x2, t2, _, _)) =
        subst x2 t1 t2
    | step (Case (Right t1, _, _, x3, t3)) =
        subst x3 t1 t3
    | step (Case (t1, x2, t2, x3, t3)) =
        step t1 >>= (fn t1' =>
        pure (Case (t1', x2, t2, x3, t3)))

    | step (t as Fun (_, _)) =
        Terminal t
    | step (App ((Fun (x, t1)), t2)) =
        subst x t2 t1
    | step (App ((App (Var f, t1)), t2)) =
        basis f [t1, t2]
    | step (App (Var f, t2)) =
        basis f [t2]
    | step (App (t1, t2)) =
        step t1 >>= (fn t1' =>
        pure (App (t1', t2)))
    | step (Var x) =
        Failure ("eval error: " ^ x ^ " is a free variable")

    | step (Let ([(x1, ty1, t1)], t)) =
        if isValue t
        then pure t
        else
          subst x1 t1 t >>= (fn t' =>
          step t'       >>= (fn t'' =>
          subst x1 t1 t'' >>= (fn t''' =>
          pure (Let ([(x1, ty1, t1)], t''')))))
  end

  fun eval t =
    case step t
      of Success t' => eval t'
       | Terminal t' => Attempt.Success t'
       | Failure m => Attempt.Failure m

end
