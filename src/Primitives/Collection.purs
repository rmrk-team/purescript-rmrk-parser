module RMRK.Primitives.Collection where

import Prelude
import Data.Argonaut.Core (Json, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import RMRK.Primitives.Address (Address)
import RMRK.Primitives.Properties (Properties)

type CollectionPayload
  = { max :: Int
    , issuer :: Address
    , symbol :: String
    , id :: CollectionId
    , metadata :: String
    }

decodeCollectionPayload :: Json -> Either JsonDecodeError CollectionPayload
decodeCollectionPayload json = decodeJson json

newtype CollectionId
  = CollectionId String

derive instance geCollectionId :: Generic CollectionId _

instance showCollectionId :: Show CollectionId where
  show = genericShow

instance eqCollectionId :: Eq CollectionId where
  eq = genericEq

instance encodeJsonCollectionId :: EncodeJson CollectionId where
  encodeJson (CollectionId a) = encodeJson a

instance decodeJsonBaseId :: DecodeJson CollectionId where
  decodeJson a = case toString a of
    Just s -> Right $ CollectionId s
    Nothing -> Left $ TypeMismatch "CollectionId"

-- export interface CollectionMetadata {
--   name?: string;
--   description?: string;
--   properties: IProperties;
--   external_url?: string;
--   image?: string;
--   image_data?: string;
-- }
type MetaData
  = { name :: Maybe String
    , description :: Maybe String
    , properties :: Properties
    , external_url :: Maybe String
    , image :: Maybe String
    , image_data :: Maybe String
    }
