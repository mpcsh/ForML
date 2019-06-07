structure Instance: sig

  type instance = {
    name: string,
    self: TyScheme.tyScheme,
    binds: (string * AST.term) list
  }

  val instanceExpr: Scan.token list -> (instance * Scan.token list) Attempt.attempt

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


  type instance = {
    name: string,
    self: TyScheme.tyScheme,
    binds: (string * AST.term) list
  }

  fun bindingExpr ts = (
    litKey "val"                       >>
    identifier                         >>= (fn name =>
    lit Scan.Equals                    >>
    Parse.expr                         >>= (fn e =>
    pure (name, e)))) ts

  fun instanceExpr ts = (
    litKey "instance"                    >>
    identifier                           >>= (fn name =>
    TyExpr.tyExpr (Scan.Keyword "where") >>=
    pure o TyScheme.generalize           >>= (fn self =>
    litKey "where"                       >>
    many1 bindingExpr                    >>= (fn binds =>
    pure {name = name, self = self, binds = binds})))) ts

end
