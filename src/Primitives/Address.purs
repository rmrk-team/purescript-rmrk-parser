module RMRK.Primitives.Address where

import Prelude
import Data.Argonaut.Core (toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

newtype Address
  = Address String

derive instance geAddress :: Generic Address _

instance showAddress :: Show Address where
  show = genericShow

instance eqAddress :: Eq Address where
  eq = genericEq

instance encodeJsonAddress :: EncodeJson Address where
  encodeJson (Address a) = encodeJson a

instance decodeJsonAddress :: DecodeJson Address where
  decodeJson a = case toString a of
    Just s -> Right $ Address s
    Nothing -> Left $ TypeMismatch "Address"
