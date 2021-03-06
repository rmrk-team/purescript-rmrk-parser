module RMRK.Syntax where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import RMRK.Primitives.Address (Address)
import RMRK.Primitives.Base (Base, BaseId, BaseSlot, BaseSlotAction, EquippableAction)
import RMRK.Primitives.Collection (CollectionId, CollectionPayload)
import RMRK.Primitives.Entity (EntityAddress)
import RMRK.Primitives.IssuableId (IssuableId)
import RMRK.Primitives.NFT (NFTBase, NFTId)
import RMRK.Primitives.Namespace (Namespace)
import RMRK.Primitives.Price (Price)
import RMRK.Primitives.Properties (AttributeValue)
import RMRK.Primitives.Recipient (Recipient)
import RMRK.Primitives.Resource (ResourcePayload)
import RMRK.Primitives.Theme (Theme, ThemeId)
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
  | CREATE Version CollectionPayload
  | ACCEPT Version NFTId EntityAddress
  | LIST Version NFTId Price
  | BURN Version NFTId
  | BUY Version NFTId (Maybe Recipient)
  | SEND Version NFTId Recipient
  | CHANGEISSUER Version IssuableId Address
  | EMOTE Version Namespace String
  | EQUIP Version NFTId BaseSlotAction
  | EQUIPPABLE Version BaseId BaseSlot EquippableAction
  | LOCK Version CollectionId
  | MINT Version NFTBase (Maybe Recipient)
  | RESADD Version NFTId ResourcePayload
  | SETPROPERTY Version NFTId String AttributeValue
  | SETPRIORITY Version NFTId (Array String)
  | THEMEADD Version BaseId ThemeId Theme

  -- rmrk::THEMEADD::{version}::{base_id}::{name}::{html_encoded_json})

derive instance genericStmt :: Generic Stmt _

instance showStmt :: Show Stmt where
  show = genericShow

instance eqStmt :: Eq Stmt where
  eq = genericEq
