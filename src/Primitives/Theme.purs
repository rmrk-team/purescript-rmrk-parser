module RMRK.Primitives.Theme
  ( Theme
  , ThemeId(..)
  , decodeThemePayload
  ) where

import Prelude

import Data.Argonaut.Core (Json, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Lib.Data.HomogenousRecord (HomogenousRecord)

type Theme
  = HomogenousRecord String

decodeThemePayload :: Json -> Either JsonDecodeError Theme
decodeThemePayload json = decodeJson json

newtype ThemeId
  = ThemeId String

derive instance geThemeId :: Generic ThemeId _

instance showThemeId :: Show ThemeId where
  show = genericShow

instance eqThemeId :: Eq ThemeId where
  eq = genericEq

instance encodeJsonThemeId :: EncodeJson ThemeId where
  encodeJson (ThemeId a) = encodeJson a

instance decodeJsonThemeId :: DecodeJson ThemeId where
  decodeJson a = case toString a of
    Just s -> Right $ ThemeId s
    Nothing -> Left $ TypeMismatch "ThemeId"
