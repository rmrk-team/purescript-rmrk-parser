module RMRK.Primitives.NFTId where

import Prelude
import Data.Argonaut.Core (toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

newtype NFTId
  = NFTId String

derive instance geNFTId :: Generic NFTId _

instance showNFTId :: Show NFTId where
  show = genericShow

instance eqNFTId :: Eq NFTId where
  eq = genericEq

instance encodeJsonNFTId :: EncodeJson NFTId where
  encodeJson (NFTId a) = encodeJson a

instance decodeJsonNFTId :: DecodeJson NFTId where
  decodeJson a = case toString a of
    Just s -> Right $ NFTId s
    Nothing -> Left $ TypeMismatch "NFTId"
