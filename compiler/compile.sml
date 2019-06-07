structure Compile : sig

  type 'a compileFn = string -> 'a Attempt.attempt

  val scan: string -> Scan.token list Attempt.attempt
  val unscan: string -> string Attempt.attempt
  val parse: string -> AST.term Attempt.attempt
  val unparse: string -> string Attempt.attempt
  val letReduce: string -> AST.term Attempt.attempt
  val annotate: string -> Annotate.aterm Attempt.attempt
  val collect: string -> (Ty.ty * Ty.ty) list Attempt.attempt
  val unify: string -> Unify.substitution Attempt.attempt
  val typeof: string -> Ty.ty Attempt.attempt
  val polish: string -> Ty.ty Attempt.attempt
  val unty: string -> string Attempt.attempt
  (* val eval: string -> AST.term Attempt.attempt *)
  (* val unval: string -> string Attempt.attempt *)

  val read: string -> string

end = struct

  open Attempt

  open AttemptMonad
  structure AttemptMonadUtil = MonadUtil(AttemptMonad)
  open AttemptMonadUtil
  infixr 0 $
  infix 1 >>= >>
  infixr 1 =<< >=> <=<
  infix 4 <$> <*>

  type 'a compileFn = string -> 'a Attempt.attempt

  fun compile pre f = fn prog =>
    pure prog >>= pre >>= f

  val scan = Scan.scan
  val unscan = compile scan (pure o Scan.unscan)

  val parse = compile scan Parse.parse
  val unparse = compile parse (pure o AST.unparse)

  val letReduce = compile parse (pure o LetReduce.reduce)

  val annotate = compile letReduce Annotate.annotate
  val collect = compile annotate Collect.collect
  val unify = compile collect Unify.unify
  fun typeof prog =
  let
    val ae = annotate prog
    val ty = Annotate.getTy =<< ae
    val sub = Unify.unify =<< Collect.collect =<< ae
  in
    liftM2 Unify.apply sub ty
  end
  fun polish prog = (
    Ty.resetTyVars ();
    compile typeof (pure o Polish.polish) prog)
  val unty = compile polish (pure o Ty.unty)
  (* fun eval prog = *)
  (* let *)
  (*   val e = parse prog *)
  (* in *)
  (*   typeof prog >> *)
  (*   e           >>= *)
  (*   Eval.eval *)
  (* end *)
  (* val unval = compile eval (pure o AST.unparse) *)

  fun read filename =
  let
    val instrm = TextIO.openIn filename
    fun lp _ =
     (case TextIO.inputLine instrm
        of NONE => ""
         | SOME line => line ^ lp ())
    val fileContentsAsString = lp ()
    val _ = TextIO.closeIn instrm
  in
    fileContentsAsString
  end

end
