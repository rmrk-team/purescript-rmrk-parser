module RMRK.Syntax.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Lib.Parsing.Combinators (Parser, bigint, fail, literal, takeuntil)
import RMRK.Primitives (NFTId(..), Price(..), Version(..))
import RMRK.Syntax (Expr(..), Stmt(..))


parser :: Parser Stmt
parser 
  = list

namespace :: Parser Expr
namespace = do 
  _ <- literal "rmrk"
  pure Namespace

seperator :: Parser Expr
seperator = do 
  _ <- literal "::"
  pure Seperator

v2 :: Parser Expr
v2 = do 
  _ <- literal "2.0.0"
  pure $ Version Two

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
  _ <- v2 
  _ <- seperator
  id <- nftid 
  _ <- seperator
  price' <- price
  pure (List Two id price')

burn :: Parser Stmt
burn = do
  _ <- namespace
  _ <- seperator
  _ <- literal "BURN"
  _ <- seperator
  _ <- v2 
  _ <- seperator
  id <- nftid 
  pure (Burn Two id)