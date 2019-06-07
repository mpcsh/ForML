structure Class: sig
 
  type class = {
    name: string,
    self: TyScheme.tyScheme,
    binds: (string * TyScheme.tyScheme) list
  }

  val classExpr: Scan.token list -> (class * Scan.token list) Attempt.attempt

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


  type class = {
    name: string,
    self: TyScheme.tyScheme,
    binds: (string * TyScheme.tyScheme) list
  }

  fun bindingExpr ts = (
    litKey "val"                       >>
    identifier                         >>= (fn name =>
    lit Scan.Colon                     >>
    TyExpr.tyExpr (Scan.Keyword "val") >>=
    pure o TyScheme.generalize         >>= (fn tyScheme =>
    pure (name, tyScheme)))) ts

  fun classExpr ts = (
    litKey "class"                       >>
    identifier                           >>= (fn name =>
    TyExpr.tyExpr (Scan.Keyword "where") >>=
    pure o TyScheme.generalize           >>= (fn self =>
    litKey "where"                       >>
    many1 bindingExpr                    >>= (fn binds =>
    pure {name = name, self = self, binds = binds})))) ts

end
