module RMRK.Primitives.Recipient where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import RMRK.Primitives.Address (Address)
import RMRK.Primitives.NFT (NFTId)

data Recipient
  = NFT NFTId
  | Account Address

derive instance geRecipient :: Generic Recipient _

instance showRecipient :: Show Recipient where
  show = genericShow

instance eqRecipient :: Eq Recipient where
  eq = genericEq
