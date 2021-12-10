module RMRK.Primitives.Block where

import Prelude
import Data.Argonaut.Core as AG
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Int (decimal, fromStringAs, toStringAs)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

newtype BlockNr
  = BlockNr Int

derive instance geBlockNr :: Generic BlockNr _

instance showBlockNr :: Show BlockNr where
  show = genericShow

instance eqBlockNr :: Eq BlockNr where
  eq = genericEq

instance encodeJsonBlockNr :: EncodeJson BlockNr where
  encodeJson (BlockNr a) = encodeJson a

instance decodeJsonBlockNr :: DecodeJson BlockNr where
  decodeJson a = do
    let
      decodedInt = fromStringAs decimal (AG.stringify a)
    case decodedInt of
      Just n -> Right $ BlockNr n
      Nothing -> Left $ TypeMismatch ("TransferableState[could not be decoded]: has to be a valid integer. Received: [" <> AG.stringify a <> "]")

toString :: BlockNr -> String
toString (BlockNr nr) = toStringAs decimal nr
