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
import Network.Wai.EventSource (ServerEvent (RetryEvent))
import Network.Wai.Test
import Okapi
import qualified Okapi (Response)
import Okapi.Test

type Okapi = OkapiT IO

testServer :: Okapi Okapi.Response
testServer = do
  let parser1 = do
        get
        pathSeg "todos"
        respond ok

      parser2 = do
        get
        path ["todos", "completed"]
        respond ok

      parser3 = do
        get
        pathSeg "todos"
        status <- queryParam "status"
        ok
          & plaintext status
          & respond

      parser4 = do
        get
        pathSeg "a"
        respond ok

      parser5 = do
        get
        pathSeg "todos"
        queryFlag "progress"
        respond ok

      parser6 = do
        get
        pathSeg ""
        respond ok

  choice
    [ parser1,
      parser2,
      parser3,
      parser4,
      parser5,
      parser6
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

  testRequest (TestRequest methodGet [] "/todos?progress" "")
    >>= assertStatus 200

  testRequest (TestRequest methodGet [] "/todos?what" "")
    >>= assertStatus 404

  testRequest (TestRequest methodGet [] "/what" "")
    >>= assertStatus 404

  testRequest (TestRequest methodGet [] "/a" "")
    >>= assertStatus 200

  testRequest (TestRequest methodGet [] "/" "")
    >>= assertStatus 200

main :: IO ()
main = Okapi.Test.runSession testSession liftIO testServer
