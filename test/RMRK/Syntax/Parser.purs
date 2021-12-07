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
import RMRK.Primitives.Base (BaseId(..), BaseType(..))
import RMRK.Primitives.Collection (CollectionId(..))
import RMRK.Primitives.Entity as EntityAddress
import RMRK.Primitives.Equippable (Equippable(..))
import RMRK.Primitives.IssuableId (IssuableId(..))
import RMRK.Primitives.NFTId (NFTId(..))
import RMRK.Primitives.Namespace (Namespace(..))
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

createCollectionPayloadJson :: String
createCollectionPayloadJson =
  """
  {
    "max": 100,
    "issuer": "CpjsLDC1JFyrhm3ftC9Gs4QoyrkHKhZKtK7YqGTRFtTafgp",
    "symbol": "DLEP",
    "id": "0aff6865bed3a66b-DLEP",
    "metadata": "ipfs://ipfs/QmVgs8P4awhZpFXhkkgnCwBp4AdKRj3F9K58mCZ6fxvn3j"
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
    describe "Create Collection" do
      it "should generally parse correctly" do
        let
          parsed = runParser parser ("rmrk::CREATE::2.0.0::" <> createCollectionPayloadJson)

          expectedPayload =
            { max: 100
            , id: CollectionId "0aff6865bed3a66b-DLEP"
            , issuer: Address "CpjsLDC1JFyrhm3ftC9Gs4QoyrkHKhZKtK7YqGTRFtTafgp"
            , symbol: "DLEP"
            , metadata: "ipfs://ipfs/QmVgs8P4awhZpFXhkkgnCwBp4AdKRj3F9K58mCZ6fxvn3j"
            }
        parsed `shouldEqual` (Right $ Tuple (CREATE V2 expectedPayload) "")
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
    describe "Change Issuer" do
      it "should parse correctly with Base id" do
        let
          parsed = runParser parser "rmrk::CHANGEISSUER::2.0.0::base-2345-SYM::somereceiveraddress"
        parsed `shouldEqual` (Right $ Tuple (CHANGEISSUER V2 (Base $ BaseId "base-2345-SYM") (Address "somereceiveraddress")) "")
      it "should parse correctly with Collection id" do
        let
          parsed = runParser parser "rmrk::CHANGEISSUER::2.0.0::0aff6865bed3a66b-KANARIA::somereceiveraddress"
        parsed `shouldEqual` (Right $ Tuple (CHANGEISSUER V2 (Collection $ CollectionId "0aff6865bed3a66b-KANARIA") (Address "somereceiveraddress")) "")
    describe "Emote" do
      it "should parse correctly with RMRK1 namepsace" do
        let
          parsed = runParser parser "rmrk::EMOTE::2.0.0::RMRK1::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::1F389"
        parsed `shouldEqual` (Right $ Tuple (EMOTE V2 (RMRK1 $ NFTId "5105000-0aff6865bed3a66b-DLEP-DL15-00000001") "1F389") "")
      it "should parse correctly with RMRK2 namepsace" do
        let
          parsed = runParser parser "rmrk::EMOTE::2.0.0::RMRK2::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::1F389"
        parsed `shouldEqual` (Right $ Tuple (EMOTE V2 (RMRK2 $ NFTId "5105000-0aff6865bed3a66b-DLEP-DL15-00000001") "1F389") "")
      it "should parse correctly with PUBKEY namepsace" do
        let
          parsed = runParser parser "rmrk::EMOTE::2.0.0::PUBKEY::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::1F389"
        parsed `shouldEqual` (Right $ Tuple (EMOTE V2 (PUBKEY "5105000-0aff6865bed3a66b-DLEP-DL15-00000001") "1F389") "")
      it "should parse correctly with EXO namepsace" do
        let
          parsed = runParser parser "rmrk::EMOTE::2.0.0::subsocial:like::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::1F389"
        parsed `shouldEqual` (Right $ Tuple (EMOTE V2 (EXO "5105000-0aff6865bed3a66b-DLEP-DL15-00000001") "1F389") "")
