module CGA.OneD where

import Prelude

import CGA.MV (class BasisVec, class CayleyTable, mkCayleyTable, mkElements)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum.Generic (genericCardinality, genericFromEnum, genericPred, genericSucc, genericToEnum)
import Data.Generic.Rep (class Generic)

data V
  = E1
  | Ep
  | Em

derive instance Eq V
derive instance Ord V
derive instance Generic V _

instance Show V where
  show = case _ of
    E1 -> "e₁"
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
    Ep -> 1.0
    Em -> -1.0
  elements = mkElements

instance CayleyTable V where
  cayleyTable = mkCayleyTable

