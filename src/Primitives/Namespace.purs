module RMRK.Primitives.Namespace
  ( Namespace(..)
  ) where

import Prelude
import Data.Argonaut.Core as A
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
  encodeJson n = encodeJson $ toString n

instance decodeJsonNamespace :: DecodeJson Namespace where
  decodeJson json = do
    let
      string = A.toString json
    case string of
      Just s -> Right $ fromString s
      Nothing -> Left $ TypeMismatch ("No matched namespace")

fromString :: String -> Namespace
fromString s = case s of
  "RMRK1" -> RMRK1
  "RMRK2" -> RMRK2
  "PUBKEY" -> PUBKEY
  s' -> EXO s'

toString :: Namespace -> String
toString n = case n of
  RMRK1 -> "RMRK1"
  RMRK2 -> "RMRK2"
  PUBKEY -> "PUBKEY"
  (EXO s) -> s
