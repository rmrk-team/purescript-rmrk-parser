module Test.Main where

import Prelude

import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (logShow)
import Lib.Parsing.Combinators (runParser)
import RMRK.Primitives (NFTId(..), Price(..), Version(..))
import RMRK.Syntax (Stmt(..))
import RMRK.Syntax.Parser (burn, list)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "RMRK.Syntax" do
    describe "List" do
      it "should generally parse correctly" do
        let parsed = runParser list "rmrk::LIST::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001::10000000000"
        let expectedPrice = BigInt.fromString "10000000000"
        case expectedPrice of 
          Just expectedPrice' -> do
            parsed `shouldEqual` (Right $ Tuple (List Two (NFTId "5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001") (PlanckPrice expectedPrice')) "")
          Nothing ->
            pure unit
      pending "feature complete"
    
    describe "Burn" do 
      it "should generally parse correctly" do
        let parsed = runParser burn "rmrk::BURN::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001"
        parsed `shouldEqual` (Right $ Tuple (Burn Two (NFTId "5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001")) "")
      pending "feature complete"