structure ParseUtil = struct

  open Attempt

  open ParseMonad
  structure ParseMonadUtil = MonadUtil(ParseMonad)
  open ParseMonadUtil
  infixr 0 $
  infix 1 >>= >>
  infixr 1 =<< >=> <=<
  infix 4 <$> <*>

  (* misc utilities *)
  infix \/
  fun p1 \/ p2 = fn ts =>
    case (p1 ts)
      of Success e => Success e
       | Failure _ => (p2 ts)

  fun many p = (fn ts => ((
    p                   >>= (fn x =>
    (many p \/ pure []) >>= (fn xs =>
    pure (x :: xs)))) \/ pure []) ts)

  fun many1 p = (fn ts => (
    p                    >>= (fn x =>
    (many1 p \/ pure []) >>= (fn xs =>
    pure (x :: xs)))) ts)

  fun lit t [] = Failure ("parse error: expected " ^ (Scan.unscan [t]) ^ ", got none")
    | lit t (t' :: ts') =
        if t = t'
        then Success ((), ts')
        else Failure
          ("expected: [" ^ (Scan.unscan [t]) ^ "]"
           ^ ", got: [" ^ (Scan.unscan [t']) ^ "]"
           ^ ", remaining program: [" ^ (Scan.unscan ts') ^ "]")

  fun litId id = lit (Scan.Identifier id)
  fun litKey key = lit (Scan.Keyword key)

  val identifier = fn ts =>
    case ts
      of Scan.Identifier x :: ts' => Success (x, ts')
       | _ => Failure ("expected identifier, got: " ^ (Scan.unscan ts))

end
