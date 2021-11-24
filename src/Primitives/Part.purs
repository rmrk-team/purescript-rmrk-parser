module RMRK.Primitives.Part where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import RMRK.Primitives.NFTId (NFTId)

type Part
  = { id :: String
    , type :: PartType
    , z :: Int
    , src :: String
    , themable :: Maybe Boolean
    , equippable :: Maybe (Array NFTId)
    }

data PartType
  = Slot
  | Fixed

derive instance gePartType :: Generic PartType _

instance showPartType :: Show PartType where
  show Slot = "slot"
  show Fixed = "fixed"

instance eqPartType :: Eq PartType where
  eq = genericEq
