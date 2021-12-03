module Test.RMRK.Syntax.Parser
  ( parsertests
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.BigInt as BigInt
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Error)
import Lib.Data.HomogenousRecord (HomogenousRecord(..))
import Lib.Parsing.Combinators (runParser)
import RMRK.Primitives.Address (Address(..))
import RMRK.Primitives.Base (BaseType(..))
import RMRK.Primitives.Entity as EntityAddress
import RMRK.Primitives.Equippable (Equippable(..))
import RMRK.Primitives.NFTId (NFTId(..))
import RMRK.Primitives.Part (PartId(..), PartType(..), Part)
import RMRK.Primitives.Price (Price(..))
import RMRK.Primitives.Recipient as Recipient
import RMRK.Primitives.ResourceId (ResourceId(..))
import RMRK.Primitives.Version (Version(..))
import RMRK.Syntax (Stmt(..))
import RMRK.Syntax.Parser (parser)
import Test.Spec (SpecT, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

basejson :: String
basejson =
  """
  { 
    "symbol": "sym", 
    "type": "svg", 
    "block": 1,
    "parts": [
      {
        "id": "partid",
        "type": "slot",
        "z": 1,
        "src": "gif.jpg",
        "themable": false,
        "equippable": ["item-1"]
      },
      {
        "id": "partid2",
        "type": "slot",
        "z": 2,
        "src": "gif2.jpg",
        "themable": true,
        "equippable": "*"
      },
      {
        "id": "partid3",
        "type": "slot",
        "z": 3,
        "src": "gif3.jpg",
        "themable": true,
        "equippable": "-"
      }
    ],
    "themes": {
      "default": {
        "color": "yellow"
      }
    }
  }
"""

expectedParts :: Array Part
expectedParts =
  [ { equippable: Just $ Items [ NFTId "item-1" ], id: (PartId "partid"), src: (Just "gif.jpg"), themable: (Just false), type: Slot, z: (Just 1) }
  , { equippable: Just $ Wildcard, id: (PartId "partid2"), src: (Just "gif2.jpg"), themable: (Just true), type: Slot, z: (Just 2) }
  , { equippable: Just $ None, id: (PartId "partid3"), src: (Just "gif3.jpg"), themable: (Just true), type: Slot, z: (Just 3) }
  ]

expectedThemes :: Maybe (HomogenousRecord (HomogenousRecord String))
expectedThemes = Just $ HomogenousRecord (M.fromFoldable [ Tuple "default" (HomogenousRecord $ M.fromFoldable [ Tuple "color" "yellow" ]) ])

parsertests :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
parsertests =
  describe "RMRK.Syntax" do
    describe "Base" do
      it "should generally parse correctly" do
        let
          parsed = runParser parser ("rmrk::BASE::2.0.0::" <> basejson)
        parsed `shouldEqual` (Right $ Tuple (BASE V2 ({ block: 1, symbol: "sym", type: SVG, parts: expectedParts, themes: expectedThemes })) "")
      pending "feature complete"
    describe "List" do
      it "should generally parse correctly" do
        let
          parsed = runParser parser "rmrk::LIST::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001::10000000000"
        let
          expectedPrice = BigInt.fromString "10000000000"
        case expectedPrice of
          Just expectedPrice' -> do
            parsed `shouldEqual` (Right $ Tuple (LIST V2 (NFTId "5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001") (PlanckPrice expectedPrice')) "")
          Nothing -> pure unit
      pending "feature complete"
    describe "Burn" do
      it "should generally parse correctly" do
        let
          parsed = runParser parser "rmrk::BURN::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001"
        parsed `shouldEqual` (Right $ Tuple (BURN V2 (NFTId "5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001")) "")
      pending "feature complete"
    describe "Buy" do
      it "should parse correctly with recipient" do
        let
          parsed = runParser parser "rmrk::BUY::2.0.0::nftid::recipientid"
        parsed `shouldEqual` (Right $ Tuple (BUY V2 (NFTId "nftid") (Just $ Recipient.Account $ Address "recipientid")) "")
      it "should parse correctly without recipient" do
        let
          parsed = runParser parser "rmrk::BUY::2.0.0::nftid"
        parsed `shouldEqual` (Right $ Tuple (BUY V2 (NFTId "nftid") Nothing) "")
      pending "feature complete"
    describe "Accept" do
      it "should parse correctly with Resource as recipient" do
        let
          parsed = runParser parser "rmrk::ACCEPT::2.0.0::nftid::RES::V1i6B"
        parsed `shouldEqual` (Right $ Tuple (ACCEPT V2 (NFTId "nftid") (EntityAddress.Resource $ ResourceId "V1i6B")) "")
      it "should parse correctly with NFT as recipient" do
        let
          parsed = runParser parser "rmrk::ACCEPT::2.0.0::nftid::NFT::nftid2"
        parsed `shouldEqual` (Right $ Tuple (ACCEPT V2 (NFTId "nftid") (EntityAddress.NFT $ NFTId "nftid2")) "")
