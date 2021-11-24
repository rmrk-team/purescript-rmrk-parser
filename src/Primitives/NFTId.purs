module RMRK.Primitives.NFTId where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

newtype NFTId
  = NFTId String

derive instance geNFTId :: Generic NFTId _

instance showNFTId :: Show NFTId where
  show = genericShow

instance eqNFTId :: Eq NFTId where
  eq = genericEq
