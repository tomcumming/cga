module Main where

import Prelude

import CGA.MV (Element, MV, elements, mv)
import CGA.OneD (V)
import Data.Foldable (for_, intercalate)
import Data.Set as S
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
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
