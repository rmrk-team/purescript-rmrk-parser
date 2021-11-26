module Lib.Data.HomogenousRecord
  ( HomogenousRecord(..)
  ) where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Encoders (encodeForeignObject)
import Data.FoldableWithIndex (foldlWithIndexDefault)
import Data.Map (Map, fromFoldable)
import Foreign.Object as F

newtype HomogenousRecord a
  = HomogenousRecord (Map String a)

instance showHomogenousRecord :: Show a => Show (HomogenousRecord a) where
  show (HomogenousRecord m) = "HomogenousRecord " <> (show m)

instance eqHomogenousRecord :: Eq a => Eq (HomogenousRecord a) where
  eq (HomogenousRecord a) (HomogenousRecord b) = a == b

instance decodeJsonHomogenousRecord :: DecodeJson a => DecodeJson (HomogenousRecord a) where
  decodeJson json = do
    obj <- decodeJson json
    let
      k = F.toUnfoldable obj :: Array _
    pure $ HomogenousRecord $ fromFoldable k

toForeignObject :: forall a. HomogenousRecord a -> F.Object a
toForeignObject (HomogenousRecord a) = foldlWithIndexDefault (\key obj v -> obj # F.insert key v) F.empty (a)

-- Object a0 -> a0 -> Object a0
instance encodeJsonHomogenousRecord :: EncodeJson a => EncodeJson (HomogenousRecord a) where
  encodeJson a = (encodeForeignObject encodeJson (toForeignObject a))
