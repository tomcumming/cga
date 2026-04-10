module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import CGA.OneD (BasisVec(..), im)

main :: Effect Unit
main = do
  let x = im E1 * im Ep * im E1
  show x # log
