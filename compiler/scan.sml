structure Scan: sig

  datatype token
    = Identifier of string
    | Keyword of string
    | LParen
    | RParen
    | Equals

    | Numeric of int
    | LBracket
    | RBracket
    | Comma
    | Quote
    | Pipe
    | Asterisk
    | Colon
    | ThinArrow
    | FatArrow

  val scan: string -> token list Attempt.attempt

  val unscan: token list -> string

end = struct

  datatype token
    = Identifier of string
    | Keyword of string
    | LParen
    | RParen
    | Equals

    | Numeric of int
    | LBracket
    | RBracket
    | Comma
    | Quote
    | Pipe
    | Asterisk
    | Colon
    | ThinArrow
    | FatArrow

  open Attempt

  open AttemptMonad
  structure AttemptMonadUtil = MonadUtil(AttemptMonad)
  open AttemptMonadUtil
  infixr 0 $
  infix 1 >>= >>
  infixr 1 =<< >=> <=<
  infix 4 <$> <*>

  fun err msg = Utils.err "scan" msg

  fun nextID cs =
  let
    fun existsIn xs x = List.exists (fn x' => x = x') xs

    val keychars = [
      #"(",
      #")",
      #"=",
      #"[",
      #"]",
      #",",
      #"+",
      #"-",
      #"\"",
      #"|",
      #">",
      #"*",
      #":"
    ]
    val isKeychar = existsIn keychars

    val keywords = [
       "fn",

       "let",
       "val",
       "in",

       "true",
       "false",

       "if",
       "then",
       "else",

       "case",
       "of",

       "class",
       "instance",
       "where"
    ]
    val isKeyword = existsIn keywords

    infix \/
    fun p \/ q = fn x => (p x orelse q x)
    fun fnot f = fn x => not (f x)

    val (cs1, cs2) = Utils.takeWhile (fnot (Char.isSpace \/ isKeychar)) cs
    val id = implode cs1
  in
    if
      id = ""
    then
      Failure ("scan error: encountered empty identifier at " ^ (implode cs))
    else if
      List.all Char.isDigit cs1
    then
      Success (Numeric (valOf (Int.fromString id)), cs2)
    else if 
      isKeyword id
    then
      Success (Keyword id, cs2)
    else
      Success (Identifier id, cs2)
  end

  fun nextToken [] = Failure "scan error: ground case"
    | nextToken ((#" " | #"\t" | #"\n") :: cs) = nextToken cs

    | nextToken (#"-" :: #">" :: cs) = Success (ThinArrow, cs)
    | nextToken (#"=" :: #">" :: cs) = Success (FatArrow, cs)

    | nextToken (#"(" :: cs) = Success (LParen, cs)
    | nextToken (#")" :: cs) = Success (RParen, cs)
    | nextToken (#"=" :: cs) = Success (Equals, cs)
    | nextToken (#"[" :: cs) = Success (LBracket, cs)
    | nextToken (#"]" :: cs) = Success (RBracket, cs)
    | nextToken (#"," :: cs) = Success (Comma, cs)
    | nextToken (#"\"" :: cs) = Success (Quote, cs)
    | nextToken (#"|" :: cs) = Success (Pipe, cs)
    | nextToken (#"*" :: cs) = Success (Asterisk, cs)
    | nextToken (#":" :: cs) = Success (Colon, cs)

    | nextToken cs = nextID cs

  fun scan program =
  let
    fun lp [] = Success []
      | lp cs =
        case nextToken cs
          of Failure "scan error: ground case" => Success []
           | Failure m => Failure m
           | Success (tok, cs') => 
               pure cs' >>=
               lp       >>= (fn cs'' =>
               pure (tok :: cs''))
  in
    lp (explode program)
  end

  fun unscanT (Identifier s) = s
    | unscanT (Keyword s) = s
    | unscanT LParen = "("
    | unscanT RParen = ")"
    | unscanT Equals = "="

    | unscanT (Numeric n) = Int.toString n
    | unscanT LBracket = "["
    | unscanT RBracket = "]"
    | unscanT Comma = ","
    | unscanT Quote = "\""
    | unscanT Pipe = "|"
    | unscanT Asterisk = "*"
    | unscanT Colon = ":"
    | unscanT ThinArrow = "->"
    | unscanT FatArrow = "=>"

  fun unscan ts =
    Utils.cat (Utils.between (map unscanT ts) ", ")

end
