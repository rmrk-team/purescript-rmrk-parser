module RMRK.Syntax.Parser where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut.Decode (parseJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Lib.Parsing.Combinators (Parser, bigint, fail, finiteString, literal, tail, takeuntil)
import RMRK.Primitives.Address (Address(..))
import RMRK.Primitives.Base (BaseId(..))
import RMRK.Primitives.Base as Base
import RMRK.Primitives.Collection (CollectionId(..))
import RMRK.Primitives.Entity (EntityAddress(..))
import RMRK.Primitives.IssuableId as IssuableId
import RMRK.Primitives.NFTId (NFTId(..))
import RMRK.Primitives.Price (Price(..))
import RMRK.Primitives.Recipient as Recipient
import RMRK.Primitives.ResourceId (ResourceId(..))
import RMRK.Primitives.Version (Version(..))
import RMRK.Syntax (Expr(..), Stmt(..))

parser :: Parser Stmt
parser = do
  _ <- namespace
  _ <- seperator
  interaction

interaction :: Parser Stmt
interaction =
  accept
    <|> list
    <|> burn
    <|> buyfor
    <|> buy
    <|> base
    <|> changeissuer

namespace :: Parser Expr
namespace = do
  _ <- literal "rmrk"
  pure Namespace

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
  id <- (takeuntil ':') <|> literal ""
  pure $ NFTId id

price :: Parser Price
price = do
  int <- bigint
  case int of
    Just bigintvalue -> pure $ PlanckPrice bigintvalue
    Nothing -> fail ("cannot parse price")

entity :: Parser EntityAddress
entity = do
  type' <- (literal "RES") <|> (literal "NFT")
  _ <- seperator
  id <- finiteString
  case type' of
    "RES" -> pure $ Resource $ ResourceId id
    "NFT" -> pure $ NFT $ NFTId id
    _ -> fail ("unrecognized type " <> type')

-- rmrk::BASE::{version}::{html_encoded_json}
base :: Parser Stmt
base = do
  _ <- literal "BASE"
  _ <- seperator
  version <- v2
  _ <- seperator
  htmlEncodedbaseJson <- tail
  case parseJson htmlEncodedbaseJson of
    Left error -> fail (printJsonDecodeError error)
    Right json -> do
      case Base.fromJson json of
        Left error' -> fail (printJsonDecodeError error')
        Right base' -> pure $ BASE version base'

accept :: Parser Stmt
accept = do
  _ <- literal "ACCEPT"
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- nftid
  _ <- seperator
  entity' <- entity
  pure (ACCEPT version id entity')

list :: Parser Stmt
list = do
  _ <- literal "LIST"
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- nftid
  _ <- seperator
  price' <- price
  pure (LIST version id price')

burn :: Parser Stmt
burn = do
  _ <- literal "BURN"
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- nftid
  pure (BURN version id)

buy :: Parser Stmt
buy = do
  _ <- literal "BUY"
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- nftid
  pure (BUY version id Nothing)

buyfor :: Parser Stmt
buyfor = do
  _ <- literal "BUY"
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- nftid
  _ <- seperator
  address <- map Address finiteString
  pure (BUY version id (Just $ Recipient.Account address))

a :: Parser String
a = pure <$> (literal "base-") <*> (takeuntil ':')

-- <|> do
--     s <- finiteString
--     pure $ Collection $ CollectionId s
issuablebaseid :: Parser IssuableId.IssuableId
issuablebaseid = (map IssuableId.Base baseid) <|> (map IssuableId.Collection collectionid)

baseid :: Parser BaseId
baseid = do
  base' <- literal "base-"
  rest <- (takeuntil ':')
  pure $ BaseId (base' <> rest)

collectionid :: Parser CollectionId
collectionid = do
  rest <- (takeuntil ':')
  pure $ CollectionId rest

changeissuer :: Parser Stmt
changeissuer = do
  _ <- literal "CHANGEISSUER"
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- issuablebaseid
  _ <- seperator
  address <- map Address finiteString
  pure $ CHANGEISSUER version id address
