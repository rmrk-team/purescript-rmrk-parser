module RMRK.Primitives.Base where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import RMRK.Primitives.Part (Part)
import RMRK.Primitives.Theme (Theme)

type Base
  = { symbol :: String
    , type :: BaseType
    , parts :: Array Part
    , themes :: Maybe (Map String Theme)
    }

data BaseType
  = SVG

derive instance geBaseType :: Generic BaseType _

instance showBaseType :: Show BaseType where
  show _ = "svg"

instance eqBaseType :: Eq BaseType where
  eq = genericEq
