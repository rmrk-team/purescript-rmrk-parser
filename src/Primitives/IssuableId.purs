module RMRK.Primitives.IssuableId where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import RMRK.Primitives.Base (BaseId)
import RMRK.Primitives.Collection (CollectionId)

data IssuableId
  = Base BaseId
  | Collection CollectionId

derive instance geIssuableId :: Generic IssuableId _

instance showIssuableId :: Show IssuableId where
  show = genericShow

instance eqIssuableId :: Eq IssuableId where
  eq = genericEq

instance encodeJsonIssuableId :: EncodeJson IssuableId where
  encodeJson a = encodeJson a

instance decodeJsonBaseId :: DecodeJson IssuableId where
  decodeJson a = decodeJson a
