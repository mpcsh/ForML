structure TyExpr: sig

  val tyExpr: Scan.token -> Scan.token list -> (Ty.ty * Scan.token list) Attempt.attempt

end = struct

  open Attempt

  open ParseMonad
  structure ParseMonadUtil = MonadUtil(ParseMonad)
  open ParseMonadUtil
  infixr 0 $
  infix 1 >>= >>
  infixr 1 =<< >=> <=<
  infix 4 <$> <*>

  datatype opers
    = Fun
    | Pair
    | List
    | Option
    | LParen

  datatype associativity = Left | Right

  fun isUnary List = true
    | isUnary Option = true
    | isUnary _ = false

  fun precedence Fun = 2
    | precedence Pair = 1
    | precedence _ = raise Fail "tried to take precedence of non-binary operator"

  fun associativity Fun = Right
    | associativity Pair = Right
    | associativity _ = raise Fail "tried to take associativity of non-binary operator"

  (* type expressions *)
  fun popFromOpers input (Fun :: ops) (x2 :: x1 :: out) =
        shunt input ops ((Ty.Fun (x1, x2)) :: out)
    | popFromOpers input (Pair :: ops) (x2 :: x1 :: out) =
        shunt input ops ((Ty.Pair (x1, x2)) :: out)
    | popFromOpers input (List :: ops) (x :: out) =
        shunt input ops ((Ty.List x) :: out)
    | popFromOpers input (Option :: ops) (x :: out) =
        shunt input ops ((Ty.Option x) :: out)

    | popFromOpers _ _ _ =
        Failure "something wasn't right with the operator stack"

    (* "number"s *)
  and shunt ((Scan.Identifier "unit") :: ts) ops out =
        shunt ts ops (Ty.Unit :: out)
    | shunt ((Scan.Identifier "bool") :: ts) ops out =
        shunt ts ops (Ty.Bool :: out)
    | shunt ((Scan.Identifier "int") :: ts) ops out =
        shunt ts ops (Ty.Int :: out)

  (* "function"s *)
    | shunt ((Scan.Identifier "list") :: ts) ops out =
        shunt ts (List :: ops) out
    | shunt ((Scan.Identifier "option") :: ts) ops out =
        shunt ts (Option :: ops) out

  (* operators *)
    | shunt ((operToken as (Scan.ThinArrow | Scan.Asterisk | Scan.Pipe)) :: ts) [] out =
      let
        val oper = case operToken
                     of Scan.ThinArrow => Fun
                      | Scan.Asterisk => Pair
                      | _ => raise Fail "something really horrible must have happened here"
      in
        shunt ts [oper] out
      end
    | shunt ((operToken as (Scan.ThinArrow | Scan.Asterisk | Scan.Pipe)) :: ts) (top :: ops) out =
      let
        val oper = case operToken
                     of Scan.ThinArrow => Fun
                      | Scan.Asterisk => Pair
                      | _ => raise Fail "something really horrible must have happened here"
      in
        if (
          (top <> LParen)
          andalso
          ((isUnary top)
          orelse ((precedence top) > (precedence oper))
          orelse (((precedence top) = (precedence oper)) andalso ((associativity top) = Left))))
        then
          popFromOpers (operToken :: ts) (top :: ops) out
        else
          shunt ts (oper :: top :: ops) out
      end

  (* parens *)
    | shunt (Scan.LParen :: ts) ops out =
        shunt ts (LParen :: ops) out

    | shunt (Scan.RParen :: ts) (LParen :: ops) out =
        shunt ts ops out
    | shunt (Scan.RParen :: ts) ops out =
        popFromOpers (Scan.RParen :: ts) ops out

  (* need to match tyvars last *)
    | shunt ((Scan.Identifier x) :: ts) ops out =
        shunt ts ops ((Ty.TyVar x) :: out)

    | shunt _ [] [] = Failure (
        "parse error: nothing on output queue at end of shunt")
    | shunt _ [] (ast :: []) = Success ast
    | shunt _ [] (_ :: _) = Failure (
        "parse error: more than one term on output stack at end of shunt\n")
    | shunt _ ops out = popFromOpers [] ops out

  fun tyExpr terminator ts =
  let
    val (ts', rest) = Utils.takeWhile (fn t => t <> terminator) ts
  in
    case (shunt ts' [] [])
      of Success ast => Success (ast, rest)
       | Failure m => Failure m
  end

end
