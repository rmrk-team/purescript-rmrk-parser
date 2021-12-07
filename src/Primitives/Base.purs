module RMRK.Primitives.Base where

import Prelude
import Data.Argonaut.Core (Json, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..), note)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(..), reflectSymbol)
import Lib.Data.HomogenousRecord (HomogenousRecord)
import RMRK.Primitives.Part (Part)

type Base
  = { symbol :: String
    , type :: BaseType
    , parts :: Array Part
    , themes :: Maybe (HomogenousRecord (HomogenousRecord String))
    , block :: Int
    }

newtype BaseId
  = BaseId String

identify :: Base -> BaseId
identify base = BaseId $ "base-" <> (show base.block) <> "-" <> base.symbol

derive instance geBaseId :: Generic BaseId _

instance showBaseId :: Show BaseId where
  show = genericShow

instance eqBaseId :: Eq BaseId where
  eq = genericEq

instance encodeJsonBaseId :: EncodeJson BaseId where
  encodeJson (BaseId a) = encodeJson a

instance decodeJsonBaseId :: DecodeJson BaseId where
  decodeJson a = case toString a of
    Just s -> Right $ BaseId s
    Nothing -> Left $ TypeMismatch "BaseId"

data BaseType
  = SVG

svgsym :: SProxy "svg"
svgsym = SProxy

fromJson :: Json -> Either JsonDecodeError Base
fromJson json = decodeJson json

baseTypeToString :: BaseType -> String
baseTypeToString _ = reflectSymbol svgsym

baseTypeFromString :: String -> Maybe BaseType
baseTypeFromString s =
  if reflectSymbol svgsym == s then
    Just SVG
  else
    Nothing

derive instance geBaseType :: Generic BaseType _

instance showBaseType :: Show BaseType where
  show _ = reflectSymbol svgsym

instance eqBaseType :: Eq BaseType where
  eq = genericEq

instance encodeJsonBaseType :: EncodeJson BaseType where
  encodeJson a = encodeJson (baseTypeToString a)

instance decodeJsonBaseType :: DecodeJson BaseType where
  decodeJson json = do
    string <- decodeJson json
    note (TypeMismatch "BaseType") (baseTypeFromString string)

data BaseSlotAction
  = Equip String
  | Unequip

derive instance geBaseSlotAction :: Generic BaseSlotAction _

instance showBaseSlotAction :: Show BaseSlotAction where
  show = genericShow

instance eqBaseSlotAction :: Eq BaseSlotAction where
  eq = genericEq
