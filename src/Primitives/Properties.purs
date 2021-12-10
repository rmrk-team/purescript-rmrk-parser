module RMRK.Primitives.Properties where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
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
    , value :: Json
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
  encodeJson a = encodeJson a

instance decodeJsonPropertyType :: DecodeJson PropertyType where
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
