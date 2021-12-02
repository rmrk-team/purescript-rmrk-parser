module RMRK.Primitives.Equippable
  ( Equippable(..)
  ) where

import Prelude
import Data.Argonaut.Core (Json, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Debug (trace)
import RMRK.Primitives.NFTId (NFTId)

data Equippable
  = Items (Array NFTId)
  | Wildcard
  | None

derive instance geEquippable :: Generic Equippable _

instance showEquippable :: Show Equippable where
  show = genericShow

instance eqEquippable :: Eq Equippable where
  eq = genericEq

instance encodeJsonEquippable :: EncodeJson Equippable where
  encodeJson Wildcard = encodeJson "*"
  encodeJson None = encodeJson "-"
  encodeJson (Items a) = encodeJson a

itemsDecoder :: Json -> Either JsonDecodeError Equippable
itemsDecoder json = case decodeJson json of
  Left error -> Left error
  Right items -> Right $ Items items

instance decodeJsonEquippable :: DecodeJson Equippable where
  decodeJson json = do
    let
      string = toString json
    case string of
      Just "*" -> Right Wildcard
      Just "-" -> Right None
      Nothing -> itemsDecoder json
      Just _ -> itemsDecoder json
