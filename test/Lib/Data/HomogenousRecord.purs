module Test.Lib.Data.HomogenousRecord where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, parseJson, printJsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Map as M
import Data.Tuple (Tuple(..))
import Effect.Aff (Error)
import Lib.Data.HomogenousRecord (HomogenousRecord(..))
import Test.Spec (SpecT, describe, it, pending)
import Test.Spec.Assertions (fail, shouldEqual)

libtests :: forall t1 t2. Monad t1 => MonadThrow Error t2 => SpecT t2 Unit t1 Unit
libtests =
  describe "Lib" do
    describe "Data" do
      describe "HomogenousRecord" do
        it "should json decode" do
          let
            jsonstring =
              """
            {
              "key": "value",
              "foo": "bar"
            }
          """
          case parseJson jsonstring of
            Left error -> fail (printJsonDecodeError error)
            Right json -> do
              let
                o :: Either JsonDecodeError (HomogenousRecord String)
                o = decodeJson json
              case o of
                Left error -> fail ((printJsonDecodeError error) <> "WAT")
                Right decoded -> decoded `shouldEqual` (HomogenousRecord $ M.fromFoldable [ (Tuple "key" "value"), (Tuple "foo" "bar") ])
        it "should json encode" do
          (stringify (encodeJson (HomogenousRecord $ M.fromFoldable [ (Tuple "key" "value"), (Tuple "foo" "bar") ]))) `shouldEqual` ("{\"foo\":\"bar\",\"key\":\"value\"}")
        pending "feature complete"
