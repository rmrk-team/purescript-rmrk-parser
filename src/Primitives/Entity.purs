module RMRK.Primitives.Entity where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import RMRK.Primitives.NFTId (NFTId)
import RMRK.Primitives.ResourceId (ResourceId)

data Entity
  = Resource ResourceId
  | NFT NFTId

derive instance geEntity :: Generic Entity _

instance showEntity :: Show Entity where
  show = genericShow

instance eqEntity :: Eq Entity where
  eq = genericEq
