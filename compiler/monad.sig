signature Monad = sig

  type 'a monad
  val pure: 'a -> 'a monad
  val >>= : 'a monad * ('a -> 'b monad) -> 'b monad

end
