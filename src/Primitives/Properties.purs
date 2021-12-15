module RMRK.Primitives.Properties where

import Prelude
import Data.Argonaut.Core (Json, stringifyWithIndent, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

type Properties
  = Map String Property

type Property
  = { _mutation ::
        Maybe
          { allowed :: Boolean
          , with ::
              Maybe
                { opType ::
                    String
                , condition ::
                    Maybe String
                }
          }
    , type :: PropertyType
    , value :: PropertyValue
    }

data PropertyType
  = Array
  | Object
  | Int
  | Float
  | String

derive instance gePropertyType :: Generic PropertyType _

instance showPropertyType :: Show PropertyType where
  show = genericShow

instance eqPropertyType :: Eq PropertyType where
  eq = genericEq

instance encodeJsonPropertyType :: EncodeJson PropertyType where
  encodeJson a = case a of
    Array -> encodeJson "array"
    Object -> encodeJson "object"
    Int -> encodeJson "int"
    Float -> encodeJson "float"
    String -> encodeJson "string"

instance decodeJsonPropertyType :: DecodeJson PropertyType where
  decodeJson json = do
    let
      string = toString json
    case string of
      Just "array" -> Right Array
      Just "object" -> Right Object
      Just "int" -> Right Int
      Just "float" -> Right Float
      Just "string" -> Right String
      Just invalid -> Left $ TypeMismatch $ "invalid property type: " <> invalid
      Nothing -> Left $ MissingValue

newtype PropertyValue
  = PropertyValue Json

derive instance gePropertyValue :: Generic PropertyValue _

instance showPropertyValue :: Show PropertyValue where
  show (PropertyValue json) = "PropertyValue " <> (stringifyWithIndent 2 json)

instance eqPropertyValue :: Eq PropertyValue where
  eq = genericEq

instance encodeJsonPropertyValue :: EncodeJson PropertyValue where
  encodeJson a = encodeJson a

instance decodeJsonPropertyValue :: DecodeJson PropertyValue where
  decodeJson a = decodeJson a

-- export type IProperties = Record<string, IAttribute>;
-- export interface IAttribute {
--   _mutation?: {
--     allowed: boolean;
--     with?: {
--       opType: OP_TYPES;
--       condition?: string;
--     };
--   };
--   type: "array" | "object" | "int" | "float" | "string";
--   value: any;
-- }
