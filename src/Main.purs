module Main where

import Prelude

import CGA.MV (Element, MV, elements, mv, reverse)
import CGA.TwoD (V(..), point, unpoint)
import Data.Foldable (for_, intercalate)
import Data.Number (cos, pi, sin)
import Data.Set as S
import Effect (Effect)
import Effect.Console (log, logShow)

main :: Effect Unit
main = do
  do
    let x = v2 2.0 3.0 # point
    let b = mv E1 * mv E2 :: MV V
    let theta = pi / 2.0 -- 90 degs
    let r = mv (cos (theta / 2.0)) + mv (sin (theta / 2.0)) * b
    let x' = r * x * reverse r
    logShow b
    logShow r
    logShow (reverse r)
    logShow x'
    logShow (unpoint x')
  where
  v2 :: Number -> Number -> MV V
  v2 x y = mv x * mv E1 + mv y * mv E2

logCayleyTable :: Effect Unit
logCayleyTable = do
  (S.toUnfoldable elements :: Array (Element V))
    # map show
    # ([ "\\" ] <> _)
    # intercalate "\t"
    # log
  for_ elements $ \(e2 :: Element V) -> do
    (S.toUnfoldable elements :: Array (Element V))
      # map (\e1 -> mv e1 * mv e2 :: MV V)
      # map show
      # ([ show e2 ] <> _)
      # intercalate "\t"
      # log
