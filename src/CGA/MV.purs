module CGA.MV
  ( class BasisVec
  , basis
  , elements
  , class CayleyTable
  , cayleyTable
  , Element
  , fromVec
  , elementGrade
  , MV
  , scaleMV
  , mkElements
  , mkCayleyTable
  , reverse
  , grade0
  , restrict
  , class IntoMV
  , mv
  ) where

import Prelude

import Data.Enum (class BoundedEnum, enumFromTo)
import Data.Foldable (fold, foldMap, foldr, intercalate)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (odd, toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Additive (Additive(Additive))
import Data.Newtype (unwrap, wrap)
import Data.Set as S
import Data.Tuple (Tuple(Tuple))
import Partial.Unsafe (unsafeCrashWith)
import Safe.Coerce (coerce)

class (Ord v, BoundedEnum v) <= BasisVec v where
  basis :: v -> Number
  elements :: S.Set (Element v)

-- Split into this else we have a cycle in mkCayleyTable
class BasisVec v <= CayleyTable v where
  cayleyTable :: Element v -> Element v -> Tuple Number (Element v)

newtype Element v = Element (S.Set v)

derive instance Eq v => Eq (Element v)

instance Ord v => Ord (Element v) where
  compare = comparing (\(Element vs) -> Tuple (S.size vs) vs)

instance Show v => Show (Element v) where
  show (Element vs)
    | S.isEmpty vs = "1"
    | otherwise = foldMap show vs

fromVec :: forall v. v -> Element v
fromVec = S.singleton >>> Element

elementGrade :: forall v. Element v -> Int
elementGrade (Element e) = S.size e

newtype MV v = MV (M.SemigroupMap (Element v) (Additive Number))

instance (Show v, Ord v) => Show (MV v) where
  show (MV m) = unwrap m
    # (M.toUnfoldable :: _ -> Array _)
    # map goPair
    # intercalate " + "
    where
    goPair = case _ of
      Tuple e (Additive n) | e == Element mempty -> goNumber n
      Tuple e (Additive n) -> goNumber n <> show e
    goNumber = case _ of
      1.0 -> "1"
      (-1.0) -> "-1"
      0.0 -> "0"
      n -> show n

scaleMV :: forall v. Number -> MV v -> MV v
scaleMV k (MV mv) = map (unwrap >>> (k * _) >>> wrap) mv # MV

powerset :: forall a. Ord a => S.Set a -> S.Set (S.Set a)
powerset = foldr (\c p -> p <> S.map (S.insert c) p) (S.singleton mempty)

mkElements :: forall v. BoundedEnum v => S.Set (Element v)
mkElements = (enumFromTo bottom top :: Array _)
  # S.fromFoldable
  # powerset
  # S.map Element

mkCayleyTable :: forall v. BasisVec v => Element v -> Element v -> Tuple Number (Element v)
mkCayleyTable = \e1 e2 -> M.lookup (Tuple e1 e2) resultMap # case _ of
  Just res -> res
  Nothing -> unsafeCrashWith "Internal error: missing in Cayley Table!"
  where
  resultMap :: M.Map (Tuple (Element v) (Element v)) (Tuple Number (Element v))
  resultMap =
    do
      e1@(Element vs1) <- S.toUnfoldable elements
      e2 <- S.toUnfoldable elements
      let res = foldr mulVec (Tuple 1.0 e2) vs1
      [ Tuple (Tuple e1 e2) res ]
      # M.fromFoldable

  mulVec :: v -> Tuple Number (Element v) -> Tuple Number (Element v)
  mulVec v (Tuple x (Element e)) = Tuple x'
    (Element (if x' == 0.0 then mempty else S.toggle v e))
    where
    x' = flipSign * square * x
    flips = S.toMap e
      # M.submap Nothing (Just v)
      # M.delete v
      # M.size
    flipSign = if odd flips then -1.0 else 1.0
    square = if S.member v e then basis v else 1.0

simplify :: forall v. BasisVec v => MV v -> MV v
simplify (MV m) = unwrap m # M.filter (_ /= Additive 0.0) # wrap # MV

instance CayleyTable v => Semiring (MV v) where
  zero = MV mempty
  one = mv 1.0
  add (MV lhs) (MV rhs) = lhs <> rhs # MV # simplify
  mul (MV lhs) (MV rhs) =
    do
      Tuple e1 (Additive x1) <- unwrap lhs # M.toUnfoldable
      Tuple e2 (Additive x2) <- unwrap rhs # M.toUnfoldable
      let (Tuple x3 e3) = cayleyTable e1 e2
      [ M.singleton e3 (Additive (x1 * x2 * x3)) # wrap ]
      # fold
      # MV
      # simplify

reverse :: forall v. MV v -> MV v
reverse (MV m) = mapWithIndex go m # MV
  where
  go e = unwrap
    >>> (_ * if mod (elementGrade e) 4 > 1 then -1.0 else 1.0)
    >>> wrap

grade0 :: forall v. BasisVec v => MV v -> Number
grade0 (MV m) = M.lookup (Element mempty) (unwrap m) # maybe 0.0 unwrap

-- | Project out the selected elements
restrict :: forall v. BasisVec v => S.Set (Element v) -> MV v -> MV v
restrict es (MV m) = unwrap m # M.filterKeys (\v -> S.member v es) # wrap # MV

instance CayleyTable v => Ring (MV v) where
  sub x = scaleMV (-1.0) >>> add x

class BasisVec v <= IntoMV v a where
  mv :: a -> MV v

instance BasisVec v => IntoMV v Number where
  mv = Additive >>> M.singleton (Element mempty) >>> wrap >>> MV
else instance BasisVec v => IntoMV v Int where
  mv = toNumber >>> mv
else instance BasisVec v => IntoMV v (Element v) where
  mv e = M.singleton e (wrap 1.0) # wrap # MV
else instance BasisVec v => IntoMV v v where
  mv v = M.singleton (S.singleton v # Element) 1.0
    # coerce
    # MV
