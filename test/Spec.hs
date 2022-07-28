{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Combinators
import Control.Monad.IO.Class
import Control.Monad.Identity
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import qualified Okapi
import Okapi.Test

type Okapi = Okapi.OkapiT IO

testServer :: Okapi Okapi.Response
testServer = do
  let parser1 = do
        Okapi.get
        Okapi.pathSeg "todos"
        Okapi.respond Okapi.ok

      parser2 = do
        Okapi.get
        Okapi.pathSeg "todos"
        Okapi.pathSeg "completed"
        Okapi.respond Okapi.ok

      parser3 = do
        Okapi.get
        Okapi.pathSeg "todos"
        status <- Okapi.queryParam @Text "status"
        Okapi.ok
          & Okapi.plaintext status
          & Okapi.respond

      parser4 = do
        Okapi.get
        Okapi.pathSeg "a"
        Okapi.respond Okapi.ok

  choice
    [ parser1,
      parser2,
      parser3,
      parser4
    ]

testSession :: Session ()
testSession = do
  testRequest (TestRequest methodGet [] "/todos" "")
    >>= assertStatus 200

  testRequest (TestRequest methodPost [] "/todos" "")
    >>= assertStatus 404

  testRequest (TestRequest methodGet [] "/todos/completed" "")
    >>= assertStatus 200

  res3 <- testRequest $ TestRequest methodGet [] "/todos?status=done" ""
  assertStatus 200 res3
  assertBody "done" res3

  testRequest (TestRequest methodGet [] "/todos?progress=finished" "")
    >>= assertStatus 404

  testRequest (TestRequest methodGet [] "/what" "")
    >>= assertStatus 404

  testRequest (TestRequest methodGet [] "/a" "")
    >>= assertStatus 200

main :: IO ()
main = Okapi.Test.runSession testSession liftIO testServer
