# purescript-rmrk-parser

### RMRK 2.0.0 protocol purescript parser

RMRK protocoll parser and primitives in purescript.

![Build & Test](https://github.com/rmrk-team/purescript-rmrk-parser/actions/workflows/build_and_test.yml/badge.svg)

**NB: Work in progress. Full spec not yet implemented.**

## Quick Example

```purs
module Main where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Lib.Parsing.Combinators (ParserError(..), runParser)
import RMRK.Primitives.Version (Version(..))
import RMRK.Syntax (Stmt(..))
import RMRK.Syntax.Parser (parser)

main :: Effect Unit
main = do
  let
    rmrk = "rmrk::MINT::2.0.0::%7B%22collection%22%3A%220aff6865bed3a66b-DLEP%22%2C%22symbol%22%3A%22DL15%22%2C%22transferable%22%3A100%2C%22sn%22%3A%2200000001%22%2C%22metadata%22%3A%22ipfs%3A%2F%2Fipfs%2FQmavoTVbVHnGEUztnBT2p3rif3qBPeCfyyUE5v4Z7oFvs4%22%7D"
  case runParser parser rmrk of
    Left (ParserError error) -> logShow error
    Right (Tuple (MINT V2 (nftbase) Nothing) "") -> logShow "mint stmt parsed"
    Right _ -> log "only mint supported by this parser"

```

## Spec

Go to the spec repo for full documentation of the RMRK v2 spec.
[rmrk-team/rmrk-spec/tree/master/standards/rmrk2.0.0](https://github.com/rmrk-team/rmrk-spec/tree/master/standards/rmrk2.0.0)

## Docs

Find the docs at [Pursuit](https://pursuit.purescript.org/packages/purescript-rmrk-parser)

## Goal

Write a dumb protocol parser that parses RMRK strings to strong rich types. Spec of protocol consolidation is not defined or handled in this package.

### Run Tests

```bash
npm run test
```
