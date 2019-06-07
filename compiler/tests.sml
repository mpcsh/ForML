structure Tests: sig

  val run: 'a Compile.compileFn -> (string * 'a Attempt.attempt) list
  val run1: string -> 'a Compile.compileFn -> 'a Attempt.attempt

end = struct

  open Attempt

  open AttemptMonad
  structure AttemptMonadUtil = MonadUtil(AttemptMonad)
  open AttemptMonadUtil
  infixr 0 $
  infix 1 >>= >>
  infixr 1 =<< >=> <=<
  infix 4 <$> <*>

  fun ls dir =
  let
    val strm = OS.FileSys.openDir dir;

    fun readAll strm =
      case OS.FileSys.readDir strm
        of SOME f => (Utils.cat [dir, "/", f]) :: (readAll strm)
         | NONE => []
  in
    readAll strm
  end

  fun run f =
  let
    val files = ls "tests"
    val ress = map (f o Compile.read) files
  in
    Utils.zipEq files ress
  end

  fun run1 file f = (f o Compile.read) (Utils.cat ["tests/", file, ".luca"])

end
