module RMRK.Primitives.Address where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype Address
  = Address String

derive instance geAddress :: Generic Address _

instance showAddress :: Show Address where
  show = genericShow

instance eqAddress :: Eq Address where
  eq = genericEq
