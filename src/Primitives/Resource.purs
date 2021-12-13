module RMRK.Primitives.Resource where

import Prelude
import Data.Argonaut.Core (Json, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

type Resource
  = { id :: ResourceId
    , src :: String
    , metadata :: String
    }

decodeResource :: Json -> Either JsonDecodeError Resource
decodeResource json = decodeJson json

newtype ResourceId
  = ResourceId String

derive instance geResourceId :: Generic ResourceId _

instance showResourceId :: Show ResourceId where
  show = genericShow

instance eqResourceId :: Eq ResourceId where
  eq = genericEq

instance encodeJsonResourceId :: EncodeJson ResourceId where
  encodeJson (ResourceId a) = encodeJson a

instance decodeJsonResourceId :: DecodeJson ResourceId where
  decodeJson a = case toString a of
    Just s -> Right $ ResourceId s
    Nothing -> Left $ TypeMismatch "ResourceId"
