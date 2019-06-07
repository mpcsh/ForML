structure LetReduce: sig

  val reduce: AST.term -> AST.term

end = struct

  local open AST in
  fun reduce Unit = Unit
    | reduce (Var x) = Var x
    | reduce True = True
    | reduce False = False
    | reduce (If (e1, e2, e3)) = crank3 e1 e2 e3 If
    | reduce Zero = Zero
    | reduce (Succ e) = crank1 e Succ
    | reduce (Pred e) = crank1 e Pred
    | reduce Nil = Nil
    | reduce (Cons (e1, e2)) = crank2 e1 e2 Cons
    | reduce (Pair (e1, e2)) = crank2 e1 e2 Pair
    | reduce None = None
    | reduce (Some e) = crank1 e Some
    | reduce (Case (e1, x2, e2, e3)) = crank3 e1 e2 e3 (fn (e1', e2', e3') => Case (e1', x2, e2', e3'))
    | reduce (Abs (x, e)) = crank1 e (fn e' => Abs (x, e'))
    | reduce (App (e1, e2)) = crank2 e1 e2 App
    | reduce (Let (x, ty, e1, e2)) = reduce (subst e1 x e2)

  and crank1 e con = con (reduce e)
  and crank2 e1 e2 con =
    let
      val e1' = reduce e1
      val e2' = reduce e2
    in
      con (e1, e2)
    end
  and crank3 e1 e2 e3 con =
    let
      val e1' = reduce e1
      val e2' = reduce e2
      val e3' = reduce e3
    in
      con (e1, e2, e3)
    end
  end

end
