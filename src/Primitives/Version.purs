module RMRK.Primitives.Version where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Version
  = V2

derive instance geVersion :: Generic Version _

instance shVersion :: Show Version where
  show = genericShow

instance eqVersion :: Eq Version where
  eq = genericEq
