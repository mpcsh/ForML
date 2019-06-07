structure Parse: sig

  val expr: Scan.token list -> (AST.term * Scan.token list) Attempt.attempt

  val parse: Scan.token list -> AST.term Attempt.attempt

end = struct

  open Attempt

  open ParseMonad
  structure ParseMonadUtil = MonadUtil(ParseMonad)
  open ParseMonadUtil
  infixr 0 $
  infix 1 >>= >>
  infixr 1 =<< >=> <=<
  infix 4 <$> <*>

  open ParseUtil
  infix 1 \/


  (* literal expressions *)
  val unitExpr =
    lit Scan.LParen >>
    lit Scan.RParen >>
    pure AST.Unit

  val varExpr = AST.Var <$> identifier

  val boolExpr =
    (litKey "true"  >> pure AST.True) \/
    (litKey "false" >> pure AST.False)

  fun toPeano 0 = AST.Zero
    | toPeano n = AST.Succ (toPeano (n - 1))
  val intExpr = fn ts =>
    case ts
      of Scan.Numeric n :: ts' => Success (toPeano n, ts')
       | _ => Failure ("expected numeric, got: " ^ (Scan.unscan ts))

  val nilExpr =
    lit Scan.LBracket >>
    lit Scan.RBracket >>
    pure AST.Nil

  val noneExpr =
    litKey "None" >>
    pure AST.None


  (* recursive expressions *)
  fun nestedExpr ts = (
    lit Scan.LParen >>
    expr            >>= (fn e1 =>
    lit Scan.RParen >>
    pure e1)) ts

  and absExpr ts = (
    litKey "fn"        >>
    (many1 identifier) >>= (fn xs =>
    lit Scan.FatArrow  >>
    expr               >>= (fn e =>
    pure (foldr AST.Abs e xs)))) ts

  and appExpr ts = (
    (varExpr \/ nestedExpr)       >>= (fn f =>
    many1 (litExpr \/ nestedExpr) >>= (fn xs =>
    pure (foldl (fn (x, f') => AST.App (f', x)) f xs)))) ts

  and letExpr ts =
  let
    fun annotated ts = (
      identifier                >>= (fn x =>
      lit Scan.Colon            >>
      TyExpr.tyExpr Scan.Equals >>= (fn ty =>
      pure (x, SOME ty)))) ts
    fun unannotated ts = (
      identifier >>= (fn x =>
      pure (x, NONE))) ts
    val bindingExpr = annotated \/ unannotated
  in (
    litKey "let"    >>
    bindingExpr     >>= (fn (x, mTy) =>
    lit Scan.Equals >>
    expr            >>= (fn e' =>
    litKey "in"     >>
    expr            >>= (fn e =>
    pure (AST.Let (x, mTy, e', e)))))) ts
  end

  and listExpr ts =
  let
    fun commaExpr ts = (
      lit Scan.Comma >>
      expr           >>= (fn e =>
      pure e)) ts
  in (
    lit Scan.LBracket >>
    expr              >>= (fn hd =>
    (many commaExpr)  >>= (fn tl =>
    lit Scan.RBracket >>
    pure (foldr (fn (x, inner) => AST.Cons (x, inner)) AST.Nil (hd :: tl))))) ts
  end

  and pairExpr ts =
  let
    fun commaExpr ts = (
      expr           >>= (fn e =>
      lit Scan.Comma >>
      pure e)) ts
  in (
    lit Scan.LParen   >>
    (many1 commaExpr) >>= (fn fsts =>
    expr              >>= (fn snd =>
    lit Scan.RParen   >>
    pure (foldr (fn (x, inner) => AST.Pair (x, inner)) snd fsts)))) ts
  end

  and ifExpr ts = (
    litKey "if"   >>
    expr          >>= (fn e1 =>
    litKey "then" >>
    expr          >>= (fn e2 =>
    litKey "else" >>
    expr          >>= (fn e3 =>
    pure (AST.If (e1, e2, e3)))))) ts

  and someExpr ts = (
    litId "Some" >>
    expr         >>= (fn e =>
    pure (AST.Some e))) ts

  and noneExpr ts = (
    litId "None" >>
    pure (AST.None)) ts

  and caseExpr ts = (
    (* case e *)
    litKey "case"           >>
    (litExpr \/ nestedExpr) >>= (fn e1 =>

    (* of Some x => e *)
    litKey "of"             >>
    litId "Some"            >>
    identifier              >>= (fn x =>
    lit Scan.FatArrow       >>
    expr                    >>= (fn e2 =>

    (* | None => e *)
    lit Scan.Pipe           >>
    litId "None"            >>
    lit Scan.FatArrow       >>
    expr                    >>= (fn e3 =>

    pure (AST.Case (e1, x, e2, e3))))))) ts

  and litExpr ts = (
    boolExpr   \/
    intExpr    \/
    listExpr   \/
    pairExpr   \/
    unitExpr   \/
    noneExpr   \/
    varExpr) ts

  and expr ts = (
    boolExpr   \/
    intExpr    \/
    listExpr   \/
    pairExpr   \/
    unitExpr   \/
    someExpr   \/
    noneExpr   \/

    ifExpr     \/
    caseExpr   \/

    absExpr    \/
    appExpr    \/
    letExpr    \/

    varExpr) ts

  fun parse tokens =
    case expr tokens
      of Success (term, []) => Success term
       | Success (term, ts) => Failure (
          "parse error: extra tokens:\n    got\n        "
          ^ (AST.unparse term)
          ^ "\n    remaining\n        "
          ^ (Scan.unscan ts))
       | Failure msg => Failure msg

end
