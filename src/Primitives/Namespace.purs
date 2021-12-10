module RMRK.Primitives.Namespace
  ( Namespace(..)
  , exo
  , exosym
  , pubkey
  , pubkeysym
  , rmrk1
  , rmrk1sym
  , rmrk2
  , rmrk2sym
  ) where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(..), reflectSymbol)
import RMRK.Primitives.NFT (NFTId)

data Namespace
  = RMRK1 NFTId
  | RMRK2 NFTId
  | PUBKEY String
  | EXO String

rmrk1sym :: SProxy "RMRK1"
rmrk1sym = SProxy

rmrk1 :: String
rmrk1 = reflectSymbol rmrk1sym

rmrk2sym :: SProxy "RMRK2"
rmrk2sym = SProxy

rmrk2 :: String
rmrk2 = reflectSymbol rmrk2sym

pubkeysym :: SProxy "PUBKEY"
pubkeysym = SProxy

pubkey :: String
pubkey = reflectSymbol pubkeysym

exosym :: SProxy "EXO"
exosym = SProxy

exo :: String
exo = reflectSymbol exosym

derive instance geNamespace :: Generic Namespace _

instance showNamespace :: Show Namespace where
  show = genericShow

instance eqNamespace :: Eq Namespace where
  eq = genericEq

-- instance encodeJsonNamespace :: EncodeJson Namespace where
--   encodeJson n = encodeJson $ toString n
-- instance decodeJsonNamespace :: DecodeJson Namespace where
--   decodeJson json = do
--     let
--       string = A.toString json
--     case string of
--       Just s -> Right $ fromString s
--       Nothing -> Left $ TypeMismatch ("No matched namespace")
-- fromString :: String -> Namespace
-- fromString s = case s of
--   "RMRK1" -> RMRK1
--   "RMRK2" -> RMRK2
--   "PUBKEY" -> PUBKEY
--   s' -> EXO s'
-- toString :: Namespace -> String
-- toString n = case n of
--   RMRK1 (NFTId id) -> id
--   RMRK2 (NFTId id) -> id
--   PUBKEY s -> s
--   (EXO s) -> s
