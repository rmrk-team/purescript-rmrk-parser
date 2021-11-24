module RMRK.Primitives.Wildcard where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (note)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))

data Wildcard
  = Wildcard

instance encodeJsonWildcard :: EncodeJson Wildcard where
  encodeJson w = encodeJson (toString w)

instance decodeJsonWildcard :: DecodeJson Wildcard where
  decodeJson json = do
    string <- decodeJson json
    note (TypeMismatch "Wildcard") (fromString string)

derive instance geWildcard :: Generic Wildcard _

instance showWildcard :: Show Wildcard where
  show _ = "*"

instance eqWildcard :: Eq Wildcard where
  eq = genericEq

fromString :: String -> Maybe Wildcard
fromString = case _ of
  "*" -> Just Wildcard
  _ -> Nothing

toString :: Wildcard -> String
toString _ = "*"
