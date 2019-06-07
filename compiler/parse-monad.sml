structure ParseMonad: Monad = struct

  open Attempt
  infix >>=

  type 'a parser = Scan.token list -> ('a * Scan.token list) attempt
  type 'a monad = 'a parser

  fun pure v = fn ts =>
    Success (v, ts)

  fun p >>= f = fn ts =>
    case (p ts)
      of Success (e, ts') => f e ts'
       | Failure m => Failure m

end
