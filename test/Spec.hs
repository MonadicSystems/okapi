{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Combinators
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types
import Network.Wai.Test
import qualified Okapi
import Network.Wai
import Data.Text
import qualified Okapi.Test

type Okapi = Okapi.OkapiT IO

testServer :: Okapi Okapi.Response
testServer = do
    let
        parser1 = do
            Okapi.get
            Okapi.pathSeg "todos"
            return Okapi.ok

        parser2 = do
            Okapi.get
            Okapi.pathSeg "todos"
            Okapi.pathSeg "completed"
            return Okapi.ok

        parser3 = do
            Okapi.get
            Okapi.pathSeg "todos"
            todoStatus <- Okapi.queryParam @Text "status"
            return Okapi.ok

        parser4 = do
            Okapi.get
            Okapi.pathSeg ""
            return Okapi.ok

    choice
        [ parser1
        , parser2
        , parser3
        , parser4
        ]

get :: BS.ByteString -> Session SResponse
get url = request $ setPath (defaultRequest { requestMethod = methodGet }) url

testSession :: Session ()
testSession = do
    response1 <- get "/todos"
    assertStatus 200 response1

    response2 <- get "/todos/completed"
    assertStatus 200 response2

    response3 <- get "/what"
    assertStatus 404 response3

    response4 <- get "/"
    assertStatus 200 response4

main :: IO ()
main = Okapi.Test.runSession testSession id testServer
