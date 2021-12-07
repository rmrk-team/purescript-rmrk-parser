module RMRK.Syntax where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import RMRK.Primitives.Address (Address)
import RMRK.Primitives.Base (Base)
import RMRK.Primitives.Collection as Collection
import RMRK.Primitives.Entity (EntityAddress)
import RMRK.Primitives.IssuableId (IssuableId)
import RMRK.Primitives.NFTId (NFTId)
import RMRK.Primitives.Namespace (Namespace)
import RMRK.Primitives.Price (Price)
import RMRK.Primitives.Recipient (Recipient)
import RMRK.Primitives.Version (Version)

data Expr
  = RootNamespace
  | Seperator
  | Version Version
  | Remark Stmt

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show = genericShow

instance eqExpr :: Eq Expr where
  eq = genericEq

data Stmt
  = BASE Version Base
  | CREATE Version Collection.CreatePayload
  | ACCEPT Version NFTId EntityAddress
  | LIST Version NFTId Price
  | BURN Version NFTId
  | BUY Version NFTId (Maybe Recipient)
  | CHANGEISSUER Version IssuableId Address
  | EMOTE Version Namespace String

derive instance genericStmt :: Generic Stmt _

instance showStmt :: Show Stmt where
  show = genericShow

instance eqStmt :: Eq Stmt where
  eq = genericEq
