{-# LANGUAGE DeriveFunctor #-}

{-# LANGUAGE RankNTypes #-}

module RecSchemes where

{- (Co)-Fixpoints and (Co)-Free Monads -}
data Fix f = In { inOp :: f(Fix f) }
data CoFix f = OutOp { out :: f (CoFix f) }

data CoFree g a = CoFree a (g (CoFree g a)) deriving Functor
data Free f a = Var a | Op (f (Free f a)) deriving Functor

coeval :: Functor g => (x -> g x) -> x -> CoFree g x
coeval coalg x = CoFree x (fmap (coeval coalg) (coalg x))

chead :: CoFree g a -> a
chead (CoFree x _) = x

ctail :: CoFree g a -> g (CoFree g a)
ctail (CoFree _ y) = y

comult :: Functor g => CoFree g a -> CoFree g (CoFree g a)
comult = coeval ctail


{- Fold -}
fold :: Functor f => (f b -> b) -> Fix f -> b
fold alg = alg . fmap (fold alg) . inOp

(/\) :: (c -> a) -> (c -> b) -> c -> (a, b)
(f /\ g) x = (f x, g x)

fmapPLeft :: (x -> x') -> (x, y) -> (x', y)
fmapPLeft f (x, y) = (f x, y)

{- Paramorphism -}
para :: Functor f => (f (b, Fix f) -> b) -> Fix f -> b
para alg = alg . (fmap ((para alg) /\ id)) . inOp

{- Fold with parameters -}
{- (- x A) ⊣ (-)^{A} -}
foldParam :: Functor f => ((f (a -> b), a) -> b) -> (Fix f, a) -> b
foldParam alg = alg . fmapPLeft (fmap ((\g -> (foldParam alg) . g) . (\x y -> (x,y))) . inOp)

paramWithParam :: Functor f =>  (((f (p -> y), Fix f), p) -> y) -> (Fix f, p) -> y
paramWithParam alg = alg . fmapPLeft ((fmap (\x' -> (\p -> (paramWithParam alg (x', p)))) . inOp) /\ id)

proj1 :: (a, b) -> a
proj1 (x, y) = x

{- Left Mutumorphism -}
mutu :: Functor f => (f (a, b) -> a) -> (f (a, b) -> b) -> Fix f -> (a, b)
mutu algLeft algRight = (algLeft /\ algRight) . fmap (mutu algLeft algRight) . inOp

{- Recursion Schemes from CoMonads -}
rsfc :: forall d g b. (Functor g, Functor d) =>
                (forall x. d (CoFree g x) -> CoFree g (d x)) ->
                (d (CoFree g b) -> b) ->
                Fix d -> b
rsfc lambda calg = chead . fold (fmap calg . lambda . (fmap comult))

{- The canonical definition from the paper -}
rsfcc :: forall g b. Functor g => (g (CoFree g b) -> b) -> Fix g -> b
rsfcc calg = chead . fold (fmap calg . lambda . (fmap comult))
  where
    lambda :: Functor g => g (CoFree g a) -> CoFree g (g a)
    lambda = fmap (fmap chead) . coeval (fmap ctail)
