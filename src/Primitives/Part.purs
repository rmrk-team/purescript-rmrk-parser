module RMRK.Primitives.Part where

import Prelude
import Data.Either (Either)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import RMRK.Primitives.NFTId (NFTId)
import RMRK.Primitives.Wildcard (Wildcard)

type Part
  = { id :: PartId
    , type :: PartType
    , z :: Maybe Int
    , src :: Maybe String
    , themable :: Maybe Boolean
    , equippable :: Maybe (Either (Array NFTId) Wildcard)
    }

newtype PartId
  = PartId String

derive instance gePartId :: Generic PartId _

instance showPartId :: Show PartId where
  show = genericShow

instance eqPartId :: Eq PartId where
  eq = genericEq

data PartType
  = Slot
  | Fixed

derive instance gePartType :: Generic PartType _

instance showPartType :: Show PartType where
  show Slot = "slot"
  show Fixed = "fixed"

instance eqPartType :: Eq PartType where
  eq = genericEq
