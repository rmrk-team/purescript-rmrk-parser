module RMRK.Primitives.Operation where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Operation
  = BUY
  | LIST
  | CREATE
  | MINT
  | SEND
  | EMOTE
  | CHANGEISSUER
  | BURN
  | BASE
  | EQUIPPABLE
  | THEMEADD
  | RESADD
  | ACCEPT
  | EQUIP
  | SETPROPERTY
  | LOCK
  | SETPRIORITY

derive instance genericOperation :: Generic Operation _

instance showOperation :: Show Operation where
  show = genericShow

instance eqOperation :: Eq Operation where
  eq = genericEq

toString :: Operation -> String
toString op = case op of
  BUY -> "BUY"
  LIST -> "LIST"
  CREATE -> "CREATE"
  MINT -> "MINT"
  SEND -> "SEND"
  EMOTE -> "EMOTE"
  CHANGEISSUER -> "CHANGEISSUER"
  BURN -> "BURN"
  BASE -> "BASE"
  EQUIPPABLE -> "EQUIPPABLE"
  THEMEADD -> "THEMEADD"
  RESADD -> "RESADD"
  ACCEPT -> "ACCEPT"
  EQUIP -> "EQUIP"
  SETPROPERTY -> "SETPROPERTY"
  LOCK -> "LOCK"
  SETPRIORITY -> "SETPRIORITY"
