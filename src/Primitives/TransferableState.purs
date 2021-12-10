module RMRK.Primitives.TransferableState where

import Prelude
import Data.Argonaut.Core as AG
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Int (decimal, fromStringAs, toStringAs)
import Data.Maybe (Maybe(..))

data TransferableState
  = Transferable
  | Nontransferable
  | AfterBlock Int
  | ForBlocks Int

instance encodeJsonTransferableState :: EncodeJson TransferableState where
  encodeJson state = encodeJson $ toInt state

instance decodeJsonTransferableState :: DecodeJson TransferableState where
  decodeJson a = case AG.toString a of
    Just s -> do
      let
        parsedInt = fromStringAs decimal s
      case parsedInt of
        Just n -> Right $ fromInt n
        Nothing -> Left $ TypeMismatch "TransferableState: has to be a valid integer."
    Nothing -> Left $ TypeMismatch "TransferableState: has to be a valid integer."

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
