module RMRK.Syntax where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import RMRK.Primitives (NFTId, Price, Recipient, Version)

data Expr
  = Namespace
  | Seperator
  | Version Version
  | Remark Stmt

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show = genericShow

instance eqExpr :: Eq Expr where
  eq = genericEq

data Stmt
  = LIST Version NFTId Price
  | BURN Version NFTId
  | BUY Version NFTId (Maybe Recipient)

derive instance genericStmt :: Generic Stmt _

instance showStmt :: Show Stmt where
  show = genericShow

instance eqStmt :: Eq Stmt where
  eq = genericEq
