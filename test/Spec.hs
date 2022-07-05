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

type Okapi = Okapi.OkapiT IO

app :: Application
app = Okapi.makeOkapiApp id 404 [] "Not Found" $ do
    let
        parser1 = do 
            Okapi.get
            Okapi.pathSeg "todos"
            Okapi.ok

        parser2 = do
            Okapi.get
            Okapi.pathSeg "todos"
            Okapi.pathSeg "completed"
            Okapi.ok

        parser3 = do
            Okapi.get
            Okapi.pathSeg "todos"
            todoStatus <- Okapi.queryParam @Text "status"
            Okapi.ok

        parser4 = do
            Okapi.get
            Okapi.pathSeg ""
            Okapi.ok

    choice
        [ parser1
        , parser2
        , parser3
        , parser4
        ]

get :: BS.ByteString -> Session SResponse
get url = request $ setPath (defaultRequest { requestMethod = methodGet }) url

tests :: Session ()
tests = do
    response1 <- get "/todos"
    assertStatus 200 response1

    response2 <- get "/todos/completed"
    assertStatus 200 response2

    response3 <- get "/what"
    assertStatus 404 response3

    response4 <- get "/"
    assertStatus 200 response4

main :: IO ()
main = runSession tests app
