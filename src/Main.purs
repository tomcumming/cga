module Main where

import Prelude

import CGA.MV (Element, MV, elements, mv)
import CGA.OneD (V)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  for_ elements $ \(e2 :: Element V) -> do
    "----- " <> show e2 # log
    for_ elements $ \(e1 :: Element V) -> do
      let res = mv e1 * mv e2 :: MV V
      show e1 <> "\t -> " <> show res # log
    log ""
