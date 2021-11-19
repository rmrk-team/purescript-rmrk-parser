module RMRK.Syntax where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import RMRK.Primitives (NFTId, Price, Version)

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
  = List Version NFTId Price
  | Burn Version NFTId

derive instance genericStmt :: Generic Stmt _

instance showStmt :: Show Stmt where
  show = genericShow

instance eqStmt :: Eq Stmt where
  eq = genericEq
