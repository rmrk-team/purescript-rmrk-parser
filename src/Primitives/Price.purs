module RMRK.Primitives.Price where

import Prelude
import Data.BigInt (BigInt)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype Price
  = PlanckPrice BigInt

derive instance gePrice :: Generic Price _

instance showPrice :: Show Price where
  show = genericShow

instance eqPrice :: Eq Price where
  eq = genericEq
