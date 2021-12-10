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

The following **interactions** are implemented:
- [x] [ACCEPT](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/accept.md) (Accept the addition of a new resource to an existing NFT, or
      the additiona of a child into a parent NFT)
- [x] [BASE](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/base.md) (Create a [Base](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/entities/base.md))
- [x] [BUY](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/buy.md) (Buy an NFT)
- [x] [CHANGEISSUER](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/changeissuer.md) (Changing the issuer of a collection or base)
- [x] [BURN](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/burn.md) (Burn an NFT)
- [x] [CREATE](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/create.md) (Minting a collection of NFTs)
- [x] [EMOTE](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/emote.md) (Send a reaction/emoticon)
- [x] [EQUIP](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/equip.md) (Equip a child NFT into a parent's slot, or unequip)
- [x] [EQUIPPABLE](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/equippable.md) (Changes the list of equippable collections on a
      base's part)
- [x] [LOCK](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/lock.md) (Locking a collection)
- [x] [MINT](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/mint.md) (Minting an NFT inside a collection)
- [ ] [LIST](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/list.md) (List an NFT for sale)
- [ ] [RESADD](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/resadd.md) (Add a new resource to an NFT as the collection issuer)
- [ ] [SEND](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/send.md) (Sending an NFT to a recipient)
- [ ] [SETPROPERTY](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/setproperty.md) (Set a custom value on an NFT)
- [ ] [SETPRIORITY](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/setpriority.md) (Set a different order of resource priority)
- [ ] [THEMEADD](https://github.com/rmrk-team/rmrk-spec/blob/master/standards/rmrk2.0.0/interactions/themeadd.md) (Add a new theme to a base)



## Docs

Find the docs at [Pursuit](https://pursuit.purescript.org/packages/purescript-rmrk-parser)

## Goal

Write a dumb protocol parser that parses RMRK strings to strong rich types. Spec of protocol consolidation is not defined or handled in this package.

### Run Tests

```bash
npm run test
```
