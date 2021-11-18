module RMRK.Syntax.Parser where

import Prelude

import Lib.Parsing.Combinators (Parser, finiteString, integer, literal)
import RMRK.Primitives (NFTId(..), Price(..), Version(..))
import RMRK.Syntax (Remark(..))


parser :: Parser Remark
parser 
  = list

rmrk :: Parser Remark
rmrk = do 
  _ <- literal "rmrk"
  pure Rmrk

seperator :: Parser Remark
seperator = do 
  _ <- literal "::"
  pure Seperator

v2 :: Parser Remark
v2 = do 
  _ <- literal "2.0.0"
  pure Version

nftid :: Parser NFTId
nftid = do 
  id <- finiteString
  pure $ NFTId id

price :: Parser Price
price = do 
  int <- integer
  pure $ PlanckPrice int

list :: Parser Remark
list = do
  _ <- rmrk
  _ <- seperator
  _ <- literal "LIST"
  _ <- seperator
  _ <- v2 
  _ <- seperator
  id <- nftid
  _ <- seperator
  price' <- price
  pure (List Two id price')