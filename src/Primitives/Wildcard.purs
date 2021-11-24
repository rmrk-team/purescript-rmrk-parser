module RMRK.Primitives.Wildcard where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Either (note)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))

data Wildcard
  = Wildcard

instance decodeJsonWildcard :: DecodeJson Wildcard where
  decodeJson json = do
    string <- decodeJson json
    note (TypeMismatch "Team") (wildcardFromString string)

derive instance geWildcard :: Generic Wildcard _

instance showWildcard :: Show Wildcard where
  show _ = "*"

instance eqWildcard :: Eq Wildcard where
  eq = genericEq

wildcardFromString :: String -> Maybe Wildcard
wildcardFromString = case _ of
  "*" -> Just Wildcard
  _ -> Nothing
