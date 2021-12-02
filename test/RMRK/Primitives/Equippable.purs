module Test.RMRK.Primitives.Equippable where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut.Core (fromString, stringify, toString)
import Data.Argonaut.Decode (decodeJson, parseJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug (trace)
import Effect.Aff (Error)
import Lib.Data.HomogenousRecord (HomogenousRecord(..))
import RMRK.Primitives.Equippable (Equippable(..))
import RMRK.Primitives.NFTId (NFTId(..))
import Test.Spec (SpecT, describe, it, pending)
import Test.Spec.Assertions (fail, shouldEqual)

equippableTests :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
equippableTests =
  describe "RMRK" do
    describe "Primitives" do
      describe "Equippable" do
        it "should json decode wildcards" do
          case decodeJson (fromString "*") of
            Left error -> fail $ printJsonDecodeError error
            Right obj -> obj `shouldEqual` Wildcard
        it "should json encode wildcards" do
          (toString $ encodeJson Wildcard) `shouldEqual` (toString $ fromString "*")
        it "should json decode none" do
          case decodeJson (fromString "-") of
            Left error -> fail $ printJsonDecodeError error
            Right obj -> obj `shouldEqual` None
        it "should json encode none" do
          (toString $ encodeJson None) `shouldEqual` (toString $ fromString "-")
        it "should json decode array of NFTId" do
          case parseJson """[ "nft-id-1" , "nft-id-2" ]""" of
            Left error -> fail (printJsonDecodeError error)
            Right json -> do
              case decodeJson json of
                Left error -> fail $ printJsonDecodeError error
                Right obj -> obj `shouldEqual` [ NFTId "nft-id-1", NFTId "nft-id-2" ]
        it "should json encode array of NFTId" do
          let
            json = encodeJson (Items [ NFTId "nft-id-1", NFTId "nft-id-2" ])
          (stringify json) `shouldEqual` ("""["nft-id-1","nft-id-2"]""")
        pending "feature complete"
