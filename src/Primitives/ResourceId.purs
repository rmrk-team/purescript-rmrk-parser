module RMRK.Primitives.Resource where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype ResourceId
  = ResourceId String

derive instance geResourceId :: Generic ResourceId _

instance showResourceId :: Show ResourceId where
  show = genericShow

instance eqResourceId :: Eq ResourceId where
  eq = genericEq
