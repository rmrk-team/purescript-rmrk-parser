module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Lib.Data.HomogenousRecord (libtests)
import Test.RMRK.Parser (parsertests)
import Test.RMRK.Primitives.Equippable (equippableTests)
import Test.Spec (pending)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        libtests
        equippableTests
        parsertests
        pending "tests completed"
