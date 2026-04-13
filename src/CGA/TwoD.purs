module CGA.TwoD where

import Prelude

import CGA.MV (class BasisVec, class CayleyTable, Element, MV, fromVec, grade0, mkCayleyTable, mkElements, mv, restrict, reverse, scaleMV)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map as M
import Data.Newtype (unwrap, wrap)
import Data.Number (abs, sqrt)
import Data.Set as S

data V
  = E1
  | E2
  | Ep
  | Em

derive instance Eq V
derive instance Ord V
derive instance Generic V _

instance Show V where
  show = case _ of
    E1 -> "e₁"
    E2 -> "e₂"
    Ep -> "e₊"
    Em -> "e₋"

instance Bounded V where
  top = genericTop
  bottom = genericBottom

instance Enum V where
  succ = genericSucc
  pred = genericPred

instance BoundedEnum V where
  cardinality = genericCardinality
  toEnum = genericToEnum
  fromEnum = genericFromEnum

instance BasisVec V where
  basis = case _ of
    E1 -> 1.0
    E2 -> 1.0
    Ep -> 1.0
    Em -> -1.0
  elements = mkElements

instance CayleyTable V where
  cayleyTable = mkCayleyTable

euclidean :: S.Set (Element V)
euclidean = S.fromFoldable [ E1, E2 ] # S.map fromVec

-- n
en :: MV V
en = mv Ep + mv Em

-- n' (n-bar)
en' :: MV V
en' = mv Ep - mv Em

point :: MV V -> MV V
point x = scaleMV 0.5 (x * x * en) + x - scaleMV 0.5 en'

unpoint :: MV V -> MV V
unpoint x = restrict euclidean x'
  where
  inner = x * en # grade0 # negate
  x' = scaleMV (1.0 / inner) x

simpleNormalise :: MV V -> MV V
simpleNormalise m = scaleMV (1.0 / s) m
  where
  s = reverse m * m # grade0 # abs # sqrt
