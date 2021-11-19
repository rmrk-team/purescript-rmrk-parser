module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Lib.Parsing.Combinators (runParser)
import RMRK.Syntax.Parser (parser)

main :: Effect Unit
main = do
  let parsed = runParser parser "rmrk::LIST::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001::10000000000"
  logShow parsed
