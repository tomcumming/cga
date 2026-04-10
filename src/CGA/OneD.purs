module CGA.OneD where

import Prelude

import Data.Array (intercalate)
import Data.Foldable (foldMap, foldr)
import Data.Int (odd)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Set as S
import Data.Tuple (Tuple(..))
import Safe.Coerce (coerce)

data BasisVec
  = E1
  | Ep
  | Em

derive instance Eq BasisVec
derive instance Ord BasisVec

instance Show BasisVec where
  show = case _ of
    E1 -> "e₁"
    Ep -> "e₊"
    Em -> "e₋"

basis :: BasisVec -> Number
basis = case _ of
  E1 -> 1.0
  Ep -> 1.0
  Em -> -1.0

newtype Element = Element (S.Set BasisVec)

derive instance Eq Element
derive instance Ord Element

instance Show Element where
  show (Element vs) = foldMap show vs

newtype MV = MV (M.SemigroupMap Element (Additive Number))

instance Show MV where
  show (MV (M.SemigroupMap m)) = (M.toUnfoldable m :: Array _)
    # map goPair
    # intercalate " + "
    where
    goPair = case _ of
      Tuple e (Additive 1.0) -> show e
      Tuple e (Additive n) -> show n <> show e

mulMV :: MV -> MV -> MV
mulMV = \(MV lhs) rhs -> foldr mulElem rhs (M.toUnfoldable (coerce lhs) :: Array _)
  where
  mulElem :: Tuple Element (Number) -> MV -> MV
  mulElem (Tuple elx x) (MV mv) = (M.toUnfoldable (coerce mv) :: Array _)
    # map (\(Tuple ely y) -> mulElems (x * y) elx ely)
    # M.fromFoldable
    # M.SemigroupMap
    # coerce
    # MV

  mulElems :: Number -> Element -> Element -> (Tuple Element (Number))
  mulElems x (Element lhs) rhs = foldr mulVec (Tuple rhs x) lhs

  mulVec :: BasisVec -> Tuple Element (Number) -> Tuple Element (Number)
  mulVec v (Tuple (Element e) y) =
    let
      flips = S.toMap e
        # M.submap Nothing (Just v)
        # M.delete v
        # M.size
      flipSign = if odd flips then -1.0 else 1.0
      square = if S.member v e then basis v else 1.0
    in
      (Tuple (Element (S.toggle v e)) (flipSign * square * y))

simplify :: MV -> MV
simplify (MV (M.SemigroupMap m)) =
  M.filter (coerce >>> (_ /= 0.0)) m # coerce # MV

instance Semiring MV where
  zero = MV mempty
  one = im 1.0
  add (MV lhs) (MV rhs) = lhs <> rhs # MV
  mul = mulMV

class IntoMV a where
  im :: a -> MV

instance IntoMV Number where
  im = M.singleton (Element mempty) >>> coerce >>> MV

instance IntoMV BasisVec where
  im v = M.singleton (S.singleton v # Element) 1.0
    # coerce
    # MV

