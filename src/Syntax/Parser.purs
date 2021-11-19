module RMRK.Syntax.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Lib.Parsing.Combinators (Parser, bigint, fail, finiteString, literal, takeuntil)
import RMRK.Primitives (Address(..), NFTId(..), Price(..), Recipient(..), Version(..))
import RMRK.Syntax (Expr(..), Stmt(..))

parser :: Parser Stmt
parser 
  =   list
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
    Nothing -> fail

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
  pure (BUY version id (Just $ Account (Address address)))
