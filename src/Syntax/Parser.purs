module RMRK.Syntax.Parser where

import Prelude
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Lib.Parsing.Combinators (Parser, bigint, fail, finiteString, literal, takeuntil)
import RMRK.Primitives.Address (Address(..))
import RMRK.Primitives.Entity (Entity(..))
import RMRK.Primitives.NFTId (NFTId(..))
import RMRK.Primitives.Price (Price(..))
import RMRK.Primitives.Recipient as Recipient
import RMRK.Primitives.ResourceId (ResourceId(..))
import RMRK.Primitives.Version (Version(..))
import RMRK.Syntax (Expr(..), Stmt(..))

parser :: Parser Stmt
parser =
  accept
    <|> list
    <|> burn
    <|> buyfor
    <|> buy

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

entity :: Parser Entity
entity = do
  type' <- (literal "RES") <|> (literal "NFT")
  _ <- seperator
  id <- finiteString
  case type' of
    "RES" -> pure $ Resource $ ResourceId id
    "NFT" -> pure $ NFT $ NFTId id
    _ -> fail ("unrecognized type " <> type')

accept :: Parser Stmt
accept = do
  _ <- namespace
  _ <- seperator
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
  _ <- namespace
  _ <- seperator
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
  _ <- namespace
  _ <- seperator
  _ <- literal "BURN"
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- nftid
  pure (BURN version id)

buy :: Parser Stmt
buy = do
  _ <- namespace
  _ <- seperator
  _ <- literal "BUY"
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- nftid
  pure (BUY version id Nothing)

buyfor :: Parser Stmt
buyfor = do
  _ <- namespace
  _ <- seperator
  _ <- literal "BUY"
  _ <- seperator
  version <- v2
  _ <- seperator
  id <- nftid
  _ <- seperator
  address <- finiteString
  pure (BUY version id (Just $ Recipient.Account (Address address)))
