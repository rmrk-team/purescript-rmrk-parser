module Test.RMRK.Syntax.Parser.Performance where

import Prelude
import Data.Array (range)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Class.Console (logShow, time, timeEnd)
import Lib.Parsing.Combinators (runParser)
import RMRK.Syntax.Parser (parser)

main :: Effect Unit
main = do
  let
    stmts = 20

    r = range 0 (20000 / stmts)
  logShow "parsing 20 000 remarks"
  _ <- time "parsing 20 000 remarks"
  for_ r \_ -> do
    let
      _ = runParser parser "rmrk::LIST::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001::10000000000"

      _ = runParser parser "rmrk::BURN::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001"

      _ = runParser parser "rmrk::BURN::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001"

      _ = runParser parser "rmrk::BUY::2.0.0::nftid::recipientid"

      _ = runParser parser "rmrk::BUY::2.0.0::nftid"

      _ = runParser parser "rmrk::ACCEPT::2.0.0::nftid::RES::V1i6B"

      _ = runParser parser "rmrk::ACCEPT::2.0.0::nftid::NFT::nftid2"

      _ = runParser parser "rmrk::CHANGEISSUER::2.0.0::base-2345-SYM::somereceiveraddress"

      _ = runParser parser "rmrk::CHANGEISSUER::2.0.0::0aff6865bed3a66b-KANARIA::somereceiveraddress"

      _ = runParser parser ("rmrk::CREATE::2.0.0::" <> createCollectionPayloadJson)

      _ = runParser parser ("rmrk::CREATE::2.0.0::" <> createCollectionPayloadJson)

      _ = runParser parser "rmrk::EMOTE::2.0.0::RMRK1::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::1F389"

      _ = runParser parser "rmrk::EMOTE::2.0.0::RMRK2::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::1F389"

      _ = runParser parser "rmrk::EMOTE::2.0.0::PUBKEY::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::1F389"

      _ = runParser parser "rmrk::EMOTE::2.0.0::subsocial:like::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::1F389"

      _ = runParser parser "rmrk::EMOTE::2.0.0::subsocial:like::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::1F389"

      _ = runParser parser "rmrk::EQUIPPABLE::2.0.0::base-575878273-kanaria_epic_birds::wing_slot_1::*"

      _ = runParser parser "rmrk::EQUIPPABLE::2.0.0::base-575878273-kanaria_epic_birds::wing_slot_1::+collection1,collection2"

      _ = runParser parser "rmrk::EQUIPPABLE::2.0.0::base-575878273-kanaria_epic_birds::wing_slot_1::-collection1,collection2"

      _ = runParser parser basejson
    pure unit
  _ <- timeEnd "parsing 20 000 remarks"
  logShow "completed"

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
