structure AttemptMonad: Monad = struct

  open Attempt
  infix >>=

  type 'a monad = 'a attempt

  val pure = Success

  fun (Failure m) >>= f = Failure m
    | (Success x) >>= f = f x

end
