module RMRK.Primitives.Part where

import Prelude
import Data.Argonaut.Core (toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..), note)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(..), reflectSymbol)
import RMRK.Primitives.NFTId (NFTId)
import RMRK.Primitives.Wildcard (Wildcard)

type Part
  = { id :: PartId
    , type :: PartType
    , z :: Maybe Int
    , src :: Maybe String
    , themable :: Maybe Boolean
    , equippable :: Maybe (Either (Array NFTId) Wildcard)
    }

newtype PartId
  = PartId String

derive instance gePartId :: Generic PartId _

instance showPartId :: Show PartId where
  show = genericShow

instance eqPartId :: Eq PartId where
  eq = genericEq

instance encodeJsonPartId :: EncodeJson PartId where
  encodeJson (PartId a) = encodeJson (partTypeFromString a)

instance decodeJsonPartId :: DecodeJson PartId where
  decodeJson a = case toString a of
    Just s -> Right $ PartId s
    Nothing -> Left $ TypeMismatch "PartId"

data PartType
  = Slot
  | Fixed

slotsym :: SProxy "slot"
slotsym = SProxy

fixedsym :: SProxy "fixed"
fixedsym = SProxy

partTypeToString :: PartType -> String
partTypeToString Slot = reflectSymbol slotsym

partTypeToString Fixed = reflectSymbol fixedsym

partTypeFromString :: String -> Maybe PartType
partTypeFromString s =
  if reflectSymbol slotsym == s then
    Just Slot
  else if reflectSymbol fixedsym == s then
    Just Fixed
  else
    Nothing

derive instance gePartType :: Generic PartType _

instance showPartType :: Show PartType where
  show Slot = "slot"
  show Fixed = "fixed"

instance eqPartType :: Eq PartType where
  eq = genericEq

instance encodeJsonBaseType :: EncodeJson PartType where
  encodeJson a = encodeJson (partTypeToString a)

instance decodeJsonBaseType :: DecodeJson PartType where
  decodeJson json = do
    string <- decodeJson json
    note (TypeMismatch "PartType") (partTypeFromString string)
