module Test.RMRK.Parser.Performance where

import Prelude
import Data.Array (range)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Class.Console (logShow, time, timeEnd)
import RMRK.Parser (parse)

main :: Effect Unit
main = do
  let
    stmts = 26

    r = range 0 (20000 / stmts)
  logShow "parsing 20 000 remarks"
  _ <- time "parsing 20 000 remarks"
  for_ r \_ -> do
    let
      _ = parse "rmrk::LIST::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001::10000000000"

      _ = parse "rmrk::BURN::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001"

      _ = parse "RMRK::BURN::2.0.0::5105000-0aff6865bed3a66b-VALHELLO-POTION_HEAL-00000001"

      _ = parse "RMRK::BUY::2.0.0::nftid::recipientid"

      _ = parse "rmrk::BUY::2.0.0::nftid"

      _ = parse "rmrk::ACCEPT::2.0.0::nftid::RES::V1i6B"

      _ = parse "RMRK::ACCEPT::2.0.0::nftid::NFT::nftid2"

      _ = parse "rmrk::CHANGEISSUER::2.0.0::base-2345-SYM::somereceiveraddress"

      _ = parse "RMRK::CHANGEISSUER::2.0.0::0aff6865bed3a66b-KANARIA::somereceiveraddress"

      _ = parse ("rmrk::CREATE::2.0.0::" <> createCollectionPayloadJson)

      _ = parse ("RMRK::create::2.0.0::" <> createCollectionPayloadJson)

      _ = parse "rmrk::EMOTE::2.0.0::RMRK1::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::1F389"

      _ = parse "RMRK::emote::2.0.0::RMRK2::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::1F389"

      _ = parse "rmrk::EMOTE::2.0.0::PUBKEY::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::1F389"

      _ = parse "rmrk::EMOTE::2.0.0::subsocial:like::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::1F389"

      _ = parse "rmrk::emote::2.0.0::subsocial:like::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::1F389"

      _ = parse "rmrk::EQUIPPABLE::2.0.0::base-575878273-kanaria_epic_birds::wing_slot_1::*"

      _ = parse "rmrk::EQUIPPABLE::2.0.0::base-575878273-kanaria_epic_birds::wing_slot_1::+collection1,collection2"

      _ = parse "RMRK::EQUIPPABLE::2.0.0::base-575878273-kanaria_epic_birds::wing_slot_1::-collection1,collection2"

      _ = parse "rmrk::MINT::2.0.0::%7B%22collection%22%3A%220aff6865bed3a66b-DLEP%22%2C%22symbol%22%3A%22DL15%22%2C%22transferable%22%3A1%2C%22sn%22%3A%2200000001%22%2C%22metadata%22%3A%22ipfs%3A%2F%2Fipfs%2FQmavoTVbVHnGEUztnBT2p3rif3qBPeCfyyUE5v4Z7oFvs4%22%7D"

      _ = parse "RMRK::mint::2.0.0::%7B%22collection%22%3A%220aff6865bed3a66b-DLEP%22%2C%22symbol%22%3A%22DL15%22%2C%22transferable%22%3A0%2C%22sn%22%3A%2200000001%22%2C%22metadata%22%3A%22ipfs%3A%2F%2Fipfs%2FQmavoTVbVHnGEUztnBT2p3rif3qBPeCfyyUE5v4Z7oFvs4%22%7D"

      _ = parse "RMRK::MINT::2.0.0::%7B%22collection%22%3A%220aff6865bed3a66b-DLEP%22%2C%22symbol%22%3A%22DL15%22%2C%22transferable%22%3A100%2C%22sn%22%3A%2200000001%22%2C%22metadata%22%3A%22ipfs%3A%2F%2Fipfs%2FQmavoTVbVHnGEUztnBT2p3rif3qBPeCfyyUE5v4Z7oFvs4%22%7D"

      _ = parse "rmrk::mint::2.0.0::%7B%22collection%22%3A%220aff6865bed3a66b-DLEP%22%2C%22symbol%22%3A%22DL15%22%2C%22transferable%22%3A-100%2C%22sn%22%3A%2200000001%22%2C%22metadata%22%3A%22ipfs%3A%2F%2Fipfs%2FQmavoTVbVHnGEUztnBT2p3rif3qBPeCfyyUE5v4Z7oFvs4%22%7D"

      _ = parse "rmrk::MINT::2.0.0::%7B%22collection%22%3A%220aff6865bed3a66b-DLEP%22%2C%22symbol%22%3A%22DL15%22%2C%22transferable%22%3A1%2C%22sn%22%3A%2200000001%22%2C%22metadata%22%3A%22ipfs%3A%2F%2Fipfs%2FQmavoTVbVHnGEUztnBT2p3rif3qBPeCfyyUE5v4Z7oFvs4%22%7D::CpjsLDC1JFyrhm3ftC9Gs4QoyrkHKhZKtK7YqGTRFtTafgp"

      _ = parse "RMRK::MINT::2.0.0::%7B%22collection%22%3A%220aff6865bed3a66b-DLEP%22%2C%22symbol%22%3A%22DL15%22%2C%22transferable%22%3A1%2C%22sn%22%3A%2200000001%22%2C%22metadata%22%3A%22ipfs%3A%2F%2Fipfs%2FQmavoTVbVHnGEUztnBT2p3rif3qBPeCfyyUE5v4Z7oFvs4%22%7D::5193445-0aff6865bed3a66b-ZOMB-ZOMBBLUE-00000001"

      _ = parse basejson
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
