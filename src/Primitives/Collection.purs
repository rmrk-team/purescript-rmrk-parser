module RMRK.Primitives.Collection where

import Prelude
import Data.Argonaut.Core (toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

newtype CollectionId
  = CollectionId String

-- identify :: Base -> BaseId
-- identify base = BaseId $ "base-" <> (show base.block) <> "-" <> base.symbol
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
