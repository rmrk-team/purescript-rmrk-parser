module RMRK.Parser
  ( accept
  , base
  , baseid
  , burn
  , buy
  , changeissuer
  , collectionid
  , entity
  , equip
  , interaction
  , issuablebaseid
  , list
  , nftid
  , parse
  , parser
  , price
  , root
  , seperator
  , v2
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (fromString)
import Data.Argonaut.Decode (parseJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), length, replace, split, toLower, trim)
import Data.String.Utils (startsWith)
import Data.Tuple (Tuple(..))
import JSURI (decodeURIComponent)
import Lib.Parsing.Combinators (Parser, ParserError, bigint, fail, finiteString, ignorecase, literal, runParser, tail, takeuntil)
import RMRK.Primitives.Address (Address(..))
import RMRK.Primitives.Base (BaseId(..), BaseSlot(..), BaseSlotAction(..), EquippableAction(..))
import RMRK.Primitives.Base as Base
import RMRK.Primitives.Collection (CollectionId(..), decodeCollectionPayload)
import RMRK.Primitives.Entity (EntityAddress(..))
import RMRK.Primitives.IssuableId as IssuableId
import RMRK.Primitives.NFT (NFTId(..), decodeNFTbase, isnftid)
import RMRK.Primitives.Namespace (Namespace(..))
import RMRK.Primitives.Operation as Op
import RMRK.Primitives.Price (Price(..))
import RMRK.Primitives.Properties (AttributeValue(..))
import RMRK.Primitives.Recipient as Recipient
import RMRK.Primitives.Resource (ResourceId(..), decodeResourcePayload)
import RMRK.Primitives.Theme (ThemeId(..), decodeThemePayload)
import RMRK.Primitives.Version (Version(..))
import RMRK.Syntax (Expr(..), Stmt(..))

-- | This is the bread and butter utility of this module.
-- |  
-- | Takes a string and produces a valid RMRK.Syntax Stmt or ParserError of String
-- |
-- | ```purs
-- | parse "rmrk::BUY::2.0.0::5105000-0aff6865bed3a66b-DLEP-DL15-00000001::H9eSvWe34vQDJAWckeTHWSqSChRat8bgKHG39GC1fjvEm7y"
-- |    == ( Right
-- |          $ BUY V2 (NFTId "5105000-0aff6865bed3a66b-DLEP-DL15-00000001")
-- |              (Just $ Recipient.Account $ Address "H9eSvWe34vQDJAWckeTHWSqSChRat8bgKHG39GC1fjvEm7y")
-- |      )
-- | ```
parse :: String -> Either ParserError Stmt
parse string = case runParser parser string of
  Left error' -> Left error'
  Right (Tuple stmt _) -> Right stmt

parser :: Parser Stmt
parser = do
  _ <- root
  _ <- seperator
  interaction

interaction :: Parser Stmt
interaction =
  accept
    <|> create
    <|> list
    <|> burn
    <|> buy
    <|> send
    <|> base
    <|> changeissuer
    <|> emote
    <|> equip
    <|> equippable
    <|> lock
    <|> mint
    <|> setpriority
    <|> resadd
    <|> setproperty
    <|> themeadd

root :: Parser Expr
root = do
  _ <- literal "RMRK" <|> literal "rmrk"
  pure RootNamespace

seperator :: Parser Expr
seperator = do
  _ <- literal "::"
  pure Seperator

v2 :: Parser Version
v2 = do
  _ <- literal "2.0.0"
  pure $ V2

nftid :: Parser NFTId
nftid = do
  id <- (takeuntil "::") <|> literal ""
  pure $ NFTId id

price :: Parser Price
price = do
  int <- bigint
  case int of
    Just bigintvalue -> pure $ PlanckPrice bigintvalue
    Nothing -> fail ("cannot parse price")

entity :: Parser EntityAddress
entity = do
  type' <- (ignorecase "RES") <|> (ignorecase "NFT")
  _ <- seperator
  id <- finiteString
  case type' of
    "RES" -> pure $ Resource $ ResourceId id
    "NFT" -> pure $ NFT $ NFTId id
    _ -> fail ("unrecognized type " <> type')

base :: Parser Stmt
base = do
  _ <- ignorecase $ Op.toString Op.BASE
  _ <- seperator
  version <- v2
  _ <- seperator
  htmlEncodedbaseJson <- tail
  case decodeURIComponent htmlEncodedbaseJson of
    Nothing -> do fail "could not url decode base json"
    Just baseJson -> case parseJson baseJson of
      Left error -> fail (printJsonDecodeError error)
      Right json -> do
        case Base.fromJson json of
          Left error' -> fail (printJsonDecodeError error')
          Right base' -> pure $ BASE version base'

create :: Parser Stmt
create = do
  _ <- ignorecase $ Op.toString Op.CREATE
  _ <- seperator
  version <- v2
  _ <- seperator
  htmlEncodedCollectionJson <- tail
  case decodeURIComponent htmlEncodedCollectionJson of
    Nothing -> do fail "could not url decode collection json"
    Just collectionJson -> case parseJson collectionJson of
      Left error -> fail (printJsonDecodeError error)
      Right json -> do
        case decodeCollectionPayload json of
          Left error' -> fail (printJsonDecodeError error')
          Right collectionCreatePayload -> pure $ CREATE version collectionCreatePayload

accept :: Parser Stmt
accept = do
  _ <- ignorecase $ Op.toString Op.ACCEPT
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- nftid
  _ <- seperator
  entity' <- entity
  pure (ACCEPT version id entity')

list :: Parser Stmt
list = do
  _ <- ignorecase $ Op.toString Op.LIST
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- nftid
  _ <- seperator
  price' <- price
  pure (LIST version id price')

burn :: Parser Stmt
burn = do
  _ <- ignorecase $ Op.toString Op.BURN
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- nftid
  pure (BURN version id)

buy :: Parser Stmt
buy = do
  _ <- ignorecase $ Op.toString Op.BUY
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- nftid
  _ <- literal "::" <|> literal ""
  recipient <- tail
  if (length recipient) > 0 then
    pure (BUY version id (Just $ Recipient.Account $ Address recipient))
  else
    pure (BUY version id Nothing)

send :: Parser Stmt
send = do
  _ <- ignorecase $ Op.toString Op.SEND
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- nftid
  _ <- literal "::" <|> literal ""
  recipient <- tail
  if isnftid recipient then
    pure (SEND version id (Recipient.NFT $ NFTId recipient))
  else
    pure (SEND version id (Recipient.Account $ Address recipient))

issuablebaseid :: Parser IssuableId.IssuableId
issuablebaseid = (map IssuableId.Base baseid) <|> (map IssuableId.Collection collectionid)

baseid :: Parser BaseId
baseid = do
  base' <- literal "base-"
  rest <- takeuntil "::"
  pure $ BaseId (base' <> rest)

collectionid :: Parser CollectionId
collectionid = do
  rest <- takeuntil "::"
  pure $ CollectionId rest

changeissuer :: Parser Stmt
changeissuer = do
  _ <- ignorecase $ Op.toString Op.CHANGEISSUER
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- issuablebaseid
  _ <- seperator
  address <- map Address finiteString
  pure $ CHANGEISSUER version id address

namespace :: Parser Namespace
namespace = do
  namespace' <- takeuntil $ "::"
  _ <- seperator
  destination <- takeuntil $ "::"
  case namespace' of
    "RMRK1" -> pure $ RMRK1 (NFTId destination)
    "RMRK2" -> pure $ RMRK2 (NFTId destination)
    "PUBKEY" -> pure $ PUBKEY destination
    _ -> pure $ EXO destination

emote :: Parser Stmt
emote = do
  _ <- ignorecase $ Op.toString Op.EMOTE
  _ <- seperator
  version <- v2
  _ <- seperator
  namespace' <- namespace
  _ <- seperator
  emotion <- tail
  pure $ EMOTE version namespace' emotion

equip :: Parser Stmt
equip = do
  _ <- ignorecase $ Op.toString Op.EQUIP
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- nftid
  _ <- seperator
  slot <- tail
  if (length slot) > 0 then case toLower slot of
    "null" -> pure $ EQUIP version id (Unequip)
    "false" -> pure $ EQUIP version id (Unequip)
    slot' -> pure $ EQUIP version id (Equip $ BaseSlot slot')
  else
    pure $ EQUIP version id (Unequip)

equippable :: Parser Stmt
equippable = do
  _ <- ignorecase $ Op.toString Op.EQUIPPABLE
  _ <- seperator
  version <- v2
  _ <- seperator
  baseid' <- baseid
  _ <- seperator
  slot <- takeuntil $ "::"
  _ <- seperator
  collections <- tail
  let
    add = startsWith "+" collections

    remove = startsWith "-" collections

    any = collections == "*"

    ids = collections # replace (Pattern (if add then "+" else "-")) (Replacement "") # trim # split (Pattern ",")

    action =
      if add then
        Right $ (MakeEquippable (CollectionId `map` ids))
      else if remove then
        Right $ (MakeUnequippable (CollectionId `map` ids))
      else if any then
        Right $ Any
      else
        Left ("Cannot parse collections string: " <> collections)
  case action of
    Right action' -> pure $ EQUIPPABLE version baseid' (BaseSlot slot) action'
    Left error -> fail error

lock :: Parser Stmt
lock = do
  _ <- ignorecase $ Op.toString Op.LOCK
  _ <- seperator
  version <- v2
  _ <- seperator
  collectionid' <- collectionid
  pure $ LOCK version collectionid'

mint :: Parser Stmt
mint = do
  _ <- ignorecase $ Op.toString Op.MINT
  _ <- seperator
  version <- v2
  _ <- seperator
  (mintforrecipient version) <|> (mintforself version)

mintforself :: Version -> Parser Stmt
mintforself version = do
  htmlEncodedNFTnJson <- takeuntil $ "::"
  case decodeURIComponent htmlEncodedNFTnJson of
    Nothing -> do fail "could not url decode NFT json"
    Just nftjson -> case parseJson nftjson of
      Left error -> fail (printJsonDecodeError error)
      Right json -> do
        case decodeNFTbase json of
          Left error' -> fail (printJsonDecodeError error')
          Right mintPayload -> pure $ MINT version mintPayload Nothing

mintforrecipient :: Version -> Parser Stmt
mintforrecipient version = do
  htmlEncodedNFTnJson <- takeuntil $ "::"
  _ <- seperator
  recipient <- tail
  case decodeURIComponent htmlEncodedNFTnJson of
    Nothing -> do fail "could not url decode NFT json"
    Just nftjson -> case parseJson nftjson of
      Left error -> fail (printJsonDecodeError error)
      Right json -> do
        case decodeNFTbase json of
          Left error' -> fail (printJsonDecodeError error')
          Right mintPayload -> pure $ MINT version mintPayload (Just $ if isnftid recipient then Recipient.NFT $ NFTId recipient else Recipient.Account $ Address recipient)

resadd :: Parser Stmt
resadd = do
  _ <- ignorecase $ Op.toString Op.RESADD
  _ <- seperator
  version <- v2
  _ <- seperator
  nftid' <- nftid
  _ <- seperator
  htmlEncodedResourceJSON <- tail
  case decodeURIComponent htmlEncodedResourceJSON of
    Nothing -> do fail "could not url decode resource json"
    Just resourceJson -> case parseJson resourceJson of
      Left error -> fail (printJsonDecodeError error)
      Right json -> do
        case decodeResourcePayload json of
          Left error' -> fail (printJsonDecodeError error')
          Right resource' -> pure $ RESADD version nftid' resource'

setproperty :: Parser Stmt
setproperty = do
  _ <- ignorecase $ Op.toString Op.SETPROPERTY
  _ <- seperator
  version <- v2
  _ <- seperator
  nftid' <- nftid
  _ <- seperator
  key <- takeuntil "::"
  _ <- seperator
  value <- tail
  case parseJson value of
    Left _ -> do pure $ SETPROPERTY version nftid' key (AttributeValue (fromString value))
    Right json -> pure $ SETPROPERTY version nftid' key (AttributeValue json)

setpriority :: Parser Stmt
setpriority = do
  _ <- ignorecase $ Op.toString Op.SETPRIORITY
  _ <- seperator
  version <- v2
  _ <- seperator
  nftid' <- nftid
  _ <- seperator
  value <- tail
  pure $ SETPRIORITY version nftid' (split (Pattern ",") value)

themeid :: Parser ThemeId
themeid = do
  rest <- takeuntil "::"
  pure $ ThemeId rest

themeadd :: Parser Stmt
themeadd = do
  _ <- ignorecase $ Op.toString Op.THEMEADD
  _ <- seperator
  version <- v2
  _ <- seperator
  baseid' <- baseid
  _ <- seperator
  themeid' <- themeid
  _ <- seperator
  htmlEncodedThemeJSON <- tail
  case decodeURIComponent htmlEncodedThemeJSON of
    Nothing -> do fail "could not url decode resource json"
    Just themeJson -> case parseJson themeJson of
      Left error -> fail (printJsonDecodeError error)
      Right json -> do
        case decodeThemePayload json of
          Left error' -> fail (printJsonDecodeError error')
          Right theme' -> pure $ THEMEADD version baseid' themeid' theme'
