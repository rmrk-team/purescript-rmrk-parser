module RMRK.Primitives.Namespace
  ( Namespace(..)
  ) where

import Prelude
import Data.Argonaut.Core (toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
data Namespace
  = RMRK1
  | RMRK2
  | PUBKEY
  | EXO String

derive instance geNamespace :: Generic Namespace _

instance showNamespace :: Show Namespace where
  show = genericShow

instance eqNamespace :: Eq Namespace where
  eq = genericEq

instance encodeJsonNamespace :: EncodeJson Namespace where
  encodeJson RMRK1 = encodeJson "RMRK1"
  encodeJson RMRK2 = encodeJson "RMRK2"
  encodeJson PUBKEY = encodeJson "PUBKEY"
  encodeJson (EXO s) = encodeJson s

instance decodeJsonNamespace :: DecodeJson Namespace where
  decodeJson json = do
    let
      string = toString json
    case string of
      Just "RMRK1" -> Right RMRK1
      Just "RMRK2" -> Right RMRK2
      Just "PUBKEY" -> Right PUBKEY
      Just s -> Right $ EXO s
      Nothing -> Left $ TypeMismatch ("No matched namespace for")
