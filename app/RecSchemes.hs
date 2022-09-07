{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}

{-# LANGUAGE RankNTypes #-}

module RecSchemes where


import Prelude hiding (foldr, foldl)
import Control.Comonad

{- (Co)-Fixpoints and (Co)-Free Monads -}
data Fix f = In { inOp :: f(Fix f) }
data CoFix f = OutOp { out :: f (CoFix f) }


{- Fold -}
fold :: Functor f => (f b -> b) -> Fix f -> b
fold alg = alg . fmap (fold alg) . inOp

(/\) :: (c -> a) -> (c -> b) -> c -> (a, b)
(f /\ g) x = (f x, g x)

fmapPLeft :: (x -> x') -> (x, y) -> (x', y)
fmapPLeft f (x, y) = (f x, y)

{- Paramorphism
Δ ⊣ ×
-}
para :: Functor f => (f (b, Fix f) -> b) -> Fix f -> b
para alg = alg . (fmap ((para alg) /\ id)) . inOp

{- Fold with parameters -}
{- (- x A) ⊣ (-)^{A} -}
foldParam :: Functor f => ((f (p -> b), p) -> b) -> (Fix f, p) -> b
foldParam alg = alg . fmapPLeft (fmap ((\g -> (foldParam alg) . g) . (\x y -> (x,y))) . inOp)

{-
Paramorphisms with parameters
Δ (- x A) ⊣ (-)^{A} (×)
-}
paramWithParam :: Functor f =>  (((f (p -> y), Fix f), p) -> y) -> (Fix f, p) -> y
paramWithParam alg = alg . fmapPLeft ((fmap (\x' -> (\p -> (paramWithParam alg (x', p)))) . inOp) /\ id)

proj1 :: (a, b) -> a
proj1 (x, y) = x

{- Left Mutumorphism -}
mutu :: Functor f => (f (a, b) -> a) -> (f (a, b) -> b) -> Fix f -> (a, b)
mutu algLeft algRight = (algLeft /\ algRight) . fmap (mutu algLeft algRight) . inOp

data CoFree g a = a :< g (CoFree g a)
data Free f a = Var a | Op (f (Free f a)) deriving Functor

coiter :: Functor g => (x -> y) -> (x -> g x) -> x -> CoFree g y
coiter f coalg x = f x :< (fmap (coiter f coalg) (coalg x))

coeval :: Functor g =>  (x -> g x) -> x -> CoFree g x
coeval = coiter id

unwrap :: CoFree g a -> g (CoFree g a)
unwrap (_ :< y) = y

instance Functor g => Functor (CoFree g) where
  fmap :: (a -> b) -> CoFree g a -> CoFree g b
  fmap f (x :< k) = f x :< fmap (fmap f) k

instance Functor f => Comonad (CoFree f) where
  extract :: CoFree f a -> a
  extract (hd :< _) = hd
  duplicate :: CoFree f a -> CoFree f (CoFree f a)
  duplicate = coiter id unwrap
  extend :: (CoFree f a -> b) -> CoFree f a -> CoFree f b
  extend k = coiter k unwrap

{- Recursion Schemes from CoMonads -}
rsfc :: forall n d a. (Comonad n, Functor d) =>
                (forall x. d (n x) -> n (d x)) ->
                (d (n a) -> a) ->
                Fix d -> a
rsfc lambda alg = extract . fold (fmap alg . lambda . (fmap duplicate))

{- The canonical definition from the paper -}
histo :: forall g b. Functor g => (g (CoFree g b) -> b) -> Fix g -> b
histo calg = rsfc lambda calg
  where
    lambda :: Functor g => forall x. g (CoFree g x) -> CoFree g (g x)
    lambda = fmap (fmap extract) . coeval (fmap unwrap)

unfold :: (Functor f) => (a -> f a) -> a -> CoFix f
unfold coalg = OutOp . fmap (unfold coalg) . coalg

hylo :: (Functor f) => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg

-- Exercise, foldr, foldl and foldl using foldr
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f e []     = e
foldl f e (x:xs) = foldl f (f e x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f e []     = e
foldr f e (x:xs) = f x (foldr f e xs)

foldlR :: (b -> a -> b) -> b -> [a] -> b
foldlR f e xs = (foldr step id) xs e
  where
    step x g e' = g (f e' x)
