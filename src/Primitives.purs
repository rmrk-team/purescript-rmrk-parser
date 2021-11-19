module RMRK.Primitives where

import Prelude
import Data.BigInt (BigInt)
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

newtype NFTId
  = NFTId String

derive instance geNFTId :: Generic NFTId _

instance showNFTId :: Show NFTId where
  show = genericShow

instance eqNFTId :: Eq NFTId where
  eq = genericEq

newtype Price
  = PlanckPrice BigInt

derive instance gePrice :: Generic Price _

instance showPrice :: Show Price where
  show = genericShow

instance eqPrice :: Eq Price where
  eq = genericEq

newtype Address
  = Address String

derive instance geAddress :: Generic Address _

instance showAddress :: Show Address where
  show = genericShow

instance eqAddress :: Eq Address where
  eq = genericEq

data Recipient
  = NFT NFTId
  | Account Address

derive instance geRecipient :: Generic Recipient _

instance showRecipient :: Show Recipient where
  show = genericShow

instance eqRecipient :: Eq Recipient where
  eq = genericEq
