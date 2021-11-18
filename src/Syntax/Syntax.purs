module RMRK.Syntax where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import RMRK.Primitives (NFTId, Price, Version)

data Remark
  = Rmrk
  | Seperator
  | Version
  | List Version NFTId Price

derive instance genericRemark :: Generic Remark _

instance showRemark :: Show Remark where
  show = genericShow

instance eqRemark :: Eq Remark where
  eq = genericEq
