module RMRK.Primitives.NFT where

import Prelude
import Data.Argonaut.Core (Json, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (index)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Int (fromStringAs)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import RMRK.Primitives.Block as Block
import RMRK.Primitives.TransferableState (TransferableState, fromInt)

type NFTBaseFields
  = ( collection :: String
    , symbol :: String
    , transferable :: TransferableState
    , sn :: String
    , metadata :: String
    )

type NFTBase
  = Record NFTBaseFields

nftstandard :: String -> String -> Int -> String -> String -> NFTBase
nftstandard collection symbol transferable sn metadata =
  { collection
  , symbol
  , transferable: fromInt transferable
  , sn
  , metadata
  }

identify :: Minted NFTBaseFields -> NFTId
identify nft =
  NFTId
    ( Block.toString nft.block <> "-"
        <> nft.collection
        <> "-"
        <> nft.symbol
        <> "-"
        <> nft.sn
    )

type Minted a
  = { block :: Block.BlockNr
    | a
    }

minted :: Block.BlockNr -> String -> String -> TransferableState -> String -> String -> Minted NFTBaseFields
minted block collection symbol transferable sn metadata =
  { block
  , collection
  , symbol
  , transferable
  , sn
  , metadata
  }

mint :: Block.BlockNr -> NFTBase -> Minted NFTBaseFields
mint blocknr nft = minted blocknr nft.collection nft.symbol nft.transferable nft.sn nft.metadata

decodeNFTStandard :: Json -> Either JsonDecodeError NFTBase
decodeNFTStandard json = decodeJson json

newtype NFTId
  = NFTId String

derive instance geNFTId :: Generic NFTId _

instance showNFTId :: Show NFTId where
  show = genericShow

instance eqNFTId :: Eq NFTId where
  eq = genericEq

instance encodeJsonNFTId :: EncodeJson NFTId where
  encodeJson (NFTId a) = encodeJson a

instance decodeJsonNFTId :: DecodeJson NFTId where
  decodeJson a = case toString a of
    Just s -> Right $ NFTId s
    Nothing -> Left $ TypeMismatch "NFTId"

isnftid :: String -> Boolean
isnftid s = do
  let
    rs = regex "^([0-9]*)-([a-z0-9_]*)-([A-Z]*)-([A-Z]*)-([0-9]+)$" noFlags
  case rs of
    Right regex -> test regex s
    _ -> false

--5193445-0aff6865bed3a66b-ZOMB-ZOMBBLUE-00000001
