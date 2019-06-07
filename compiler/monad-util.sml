signature MonadUtil = sig

  include Monad

  (* A synonym for >>= *)
  val bind : 'a monad -> ('a -> 'b monad) -> 'b monad

  (* A handful of sequencing related operators. *)
  val =<< : ('a -> 'b monad) * 'a monad -> 'b monad
  val >> : 'a monad * 'b monad -> 'b monad
  val >=> : ('a -> 'b monad) * ('b -> 'c monad) -> 'a -> 'c monad
  val <=< : ('a -> 'b monad) * ('c -> 'a monad) -> 'c -> 'b monad

  (* Functions for sequencing lists. *)
  val sequence : 'a monad list -> 'a list monad
  val sequence_ : 'a monad list -> unit monad
  val mapM : ('a -> 'b monad) -> 'a list -> 'b list monad
  val mapM_ : ('a -> 'b monad) -> 'a list -> unit monad
  val forM : 'a list -> ('a -> 'b monad) -> 'b list monad
  val forM_ : 'a list -> ('a -> 'b monad) -> unit monad

  (* Functions for lifting other functions into monads. *)
  val liftM : ('a -> 'b) -> 'a monad -> 'b monad
  val liftM2 : ('a -> 'b -> 'c) -> 'a monad -> 'b monad -> 'c monad
  val liftM3 : ('a -> 'b -> 'c -> 'd) -> 'a monad -> 'b monad -> 'c monad -> 'd monad
  val liftM2' : ('a * 'b -> 'c) -> 'a monad * 'b monad -> 'c monad

  (* Things that properly belong in a functor or applicative. *)
  val fmap : ('a -> 'b) -> 'a monad -> 'b monad
  val <$> : ('a -> 'b) * 'a monad -> 'b monad
  val ap : ('a -> 'b) monad -> 'a monad -> 'b monad
  val <*> : ('a -> 'b) monad * 'a monad -> 'b monad

  val forever : 'a monad -> 'b monad
  val join : 'a monad monad -> 'a monad

end

functor MonadUtil (M: Monad): MonadUtil = struct

  open M
  infixr 0 $
  infix 1 >>= >>
  infixr 1 =<< >=> <=<
  infix 4 <$> <*>

  fun f $ x = f x
  fun id x = x

  fun bind m f = m >>= f
  fun f =<< mx = mx >>= f
  fun x >> y = x >>= (fn _ => y)

  (* fish. *)
  fun f >=> g = fn x => f x >>= g
  fun g <=< f = fn x => f x >>= g

  (* Some of these are names stolen from haskell functions
   * over Functor and Applicative instances. *)
  fun fmap f mx = mx >>= (fn x => pure $ f x)
  fun f <$> x = fmap f x
  val liftM = fmap
  fun liftM2 f m1 m2 =
      m1 >>= (fn x1 => m2 >>= (fn x2 => pure $ f x1 x2))
  fun liftM3 f m1 m2 m3 =
      m1 >>= (fn x1 => m2 >>= (fn x2 => m3 >>= (fn x3 => pure $ f x1 x2 x3)))
  (* Sigh, SML. Why don't you curry more? *)
  fun liftM2' f (m1, m2) =
      m1 >>= (fn x1 => m2 >>= (fn x2 => pure $ f (x1, x2)))

  fun ap f x = liftM2 id f x
  fun f <*> x = ap f x

  fun sequence ms =
      foldr (liftM2' (op ::)) (pure []) ms

  fun sequence_ ms = foldr (op >>) (pure ()) ms

  fun mapM f x = sequence $ map f x
  fun mapM_ f x = sequence_ $ map f x
  fun forM x f = mapM f x
  fun forM_ x f = mapM_ f x

  (* Haskell defines forever as "forever a   = a >> forever a".
   * We can't do that because we are strict *)
  fun forever a = a >>= (fn _ => forever a)

  fun join m = m >>= id

end
