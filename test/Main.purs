module Test.Main where

import Prelude

import Data.Array (range)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (time, timeEnd)
import Lib.Parsing.Combinators (runParser)
import RMRK.Primitives (Address(..), NFTId(..), Price(..), Recipient(..), Version(..))
import RMRK.Syntax (Stmt(..))
import RMRK.Syntax.Parser (parser)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "RMRK.Syntax" do
    describe "List" do
      it "should generally parse correctly" do
        let parsed = runParser parser "rmrk::LIST::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001::10000000000"
        let expectedPrice = BigInt.fromString "10000000000"
        case expectedPrice of 
          Just expectedPrice' -> do
            parsed `shouldEqual` (Right $ Tuple (LIST V2 (NFTId "5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001") (PlanckPrice expectedPrice')) "")
          Nothing ->
            pure unit
      pending "feature complete"
    
    describe "Burn" do 
      it "should generally parse correctly" do
        let parsed = runParser parser "rmrk::BURN::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001"
        parsed `shouldEqual` (Right $ Tuple (BURN V2 (NFTId "5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001")) "")
      pending "feature complete"

    describe "Buy" do 
      it "should parse correctly with recipient" do
        let parsed = runParser parser "rmrk::BUY::2.0.0::nftid::recipientid"
        parsed `shouldEqual` (Right $ Tuple (BUY V2 (NFTId "nftid") (Just $ Account $ Address "recipientid")) "")
      it "should parse correctly without recipient" do
        let parsed = runParser parser "rmrk::BUY::2.0.0::nftid"
        parsed `shouldEqual` (Right $ Tuple (BUY V2 (NFTId "nftid") Nothing) "")
      pending "feature complete"

    describe "Performance" do 
      it "parse 15 000 strings" do
        _ <- liftEffect $ time "parse 15 000 strings"
        let r = range 0 5000
        for_ r \_ -> do
          let _ = runParser parser "rmrk::BUY::2.0.0::nftid::recipientid"
          let _ = runParser parser "rmrk::LIST::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001::10000000000"
          let _ = runParser parser "rmrk::BURN::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001"
          pure unit
        _ <- liftEffect $ timeEnd "parse 15 000 strings"
        pure $ unit  
      pending "feature complete"