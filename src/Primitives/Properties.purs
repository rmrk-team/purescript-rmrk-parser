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
  = Map String Attribute

type Attribute
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
    , type :: AttributeType
    , value :: AttributeValue
    }

data AttributeType
  = Array
  | Object
  | Int
  | Float
  | String

derive instance geAttributeType :: Generic AttributeType _

instance showAttributeType :: Show AttributeType where
  show = genericShow

instance eqAttributeType :: Eq AttributeType where
  eq = genericEq

instance encodeJsonAttributeType :: EncodeJson AttributeType where
  encodeJson a = case a of
    Array -> encodeJson "array"
    Object -> encodeJson "object"
    Int -> encodeJson "int"
    Float -> encodeJson "float"
    String -> encodeJson "string"

instance decodeJsonAttributeType :: DecodeJson AttributeType where
  decodeJson json = do
    let
      string = toString json
    case string of
      Just "array" -> Right Array
      Just "object" -> Right Object
      Just "int" -> Right Int
      Just "float" -> Right Float
      Just "string" -> Right String
      Just invalid -> Left $ TypeMismatch $ "invalid Attribute type: " <> invalid
      Nothing -> Left $ MissingValue

newtype AttributeValue
  = AttributeValue Json

derive instance geAttributeValue :: Generic AttributeValue _

instance showAttributeValue :: Show AttributeValue where
  show (AttributeValue json) = "AttributeValue " <> (stringifyWithIndent 2 json)

instance eqAttributeValue :: Eq AttributeValue where
  eq = genericEq

instance encodeJsonAttributeValue :: EncodeJson AttributeValue where
  encodeJson a = encodeJson a

instance decodeJsonAttributeValue :: DecodeJson AttributeValue where
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
