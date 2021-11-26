module RMRK.Primitives.Base where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either, note)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..), reflectSymbol)
import Lib.Data.HomogenousRecord (HomogenousRecord)
import RMRK.Primitives.Part (Part)

type Base
  = { symbol :: String
    , type :: BaseType
    , parts :: Array Part
    , themes :: Maybe (HomogenousRecord (HomogenousRecord String))
    }

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
