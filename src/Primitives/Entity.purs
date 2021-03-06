module RMRK.Primitives.Entity where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import RMRK.Primitives.NFT (NFTId)
import RMRK.Primitives.Resource (ResourceId)

data EntityAddress
  = Resource ResourceId
  | NFT NFTId

derive instance geEntityAddress :: Generic EntityAddress _

instance showEntityAddress :: Show EntityAddress where
  show = genericShow

instance eqEntityAddress :: Eq EntityAddress where
  eq = genericEq
