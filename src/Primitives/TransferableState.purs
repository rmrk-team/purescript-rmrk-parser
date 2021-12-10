module RMRK.Primitives.TransferableState where

import Prelude
import Data.Argonaut.Core as AG
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Int (decimal, fromStringAs, toStringAs)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

data TransferableState
  = Transferable
  | Nontransferable
  | AfterBlock Int
  | ForBlocks Int

derive instance geTransferableState :: Generic TransferableState _

instance showTransferableState :: Show TransferableState where
  show = genericShow

instance eqTransferableState :: Eq TransferableState where
  eq = genericEq

instance encodeJsonTransferableState :: EncodeJson TransferableState where
  encodeJson state = encodeJson $ toInt state

instance decodeJsonTransferableState :: DecodeJson TransferableState where
  decodeJson a = do
    let
      decodedInt = fromStringAs decimal (AG.stringify a)
    case decodedInt of
      Just n -> Right $ fromInt n
      Nothing -> Left $ TypeMismatch ("TransferableState[could not be decoded]: has to be a valid integer. Received: [" <> AG.stringify a <> "]")

toInt :: TransferableState -> Int
toInt state = case state of
  (Transferable) -> 1
  (Nontransferable) -> 0
  (AfterBlock n) -> n
  (ForBlocks n) -> n

fromInt :: Int -> TransferableState
fromInt n =
  if n == 1 then
    Transferable
  else if n == 0 then
    Nontransferable
  else if n > 1 then
    AfterBlock n
  else
    ForBlocks (n * -1)

toString :: TransferableState -> String
toString state = toInt state # toStringAs decimal
