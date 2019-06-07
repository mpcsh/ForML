structure OptAttemptMonad: Monad = struct

  open OptAttempt
  infix >>=

  type 'a monad = 'a optattempt

  val pure = Success

  fun (Failure m) >>= f = Failure m
    | (Terminal x) >>= f = f x
    | (Success x) >>= f = f x

end
