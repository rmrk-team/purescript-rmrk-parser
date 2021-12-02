module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (time, timeEnd)
import Test.Lib.Data.HomogenousRecord (libtests)
import Test.RMRK.Primitives.Equippable (equippableTests)
import Test.RMRK.Syntax.Parser (parsertests)
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
