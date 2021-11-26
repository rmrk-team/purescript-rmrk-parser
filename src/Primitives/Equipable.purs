module RMRK.Primitives.Equippable
  ( Equippable(..)
  ) where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Decoders (decodeArray)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import RMRK.Primitives.NFTId (NFTId(..))

data Equippable
  = Items (Array NFTId)
  | Wildcard
  | None

-- derive instance geNFTId :: Generic NFTId _
-- instance showNFTId :: Show NFTId where
--   show = genericShow
-- instance eqNFTId :: Eq NFTId where
--   eq = genericEq
instance encodeJsonEquippable :: EncodeJson Equippable where
  encodeJson (Items a) = encodeJson a
  encodeJson Wildcard = encodeJson "*"
  encodeJson None = encodeJson "-"

-- instance decodeJsonEquippable :: DecodeJson Equippable where
--   decodeJson json = do
--     items' <- decodeJson json
--     items'
