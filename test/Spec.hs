{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Combinators
import Control.Monad.Identity
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types
import Network.Wai.Test
import qualified Okapi
import Network.Wai
import Data.Text
import Okapi.Test

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
            _ <- Okapi.queryParam @Text "status"
            return Okapi.ok

        parser4 = do
            Okapi.get
            Okapi.pathSeg "a"
            return Okapi.ok

    choice
        [ parser1
        , parser2
        , parser3
        , parser4
        ]

testSession :: Session ()
testSession = do
    (testRequest $ TestRequest methodGet [] "/todos" "")
        >>= assertStatus 200

    (testRequest $ TestRequest methodGet [] "/todos/completed" "")
        >>= assertStatus 200

    (testRequest $ TestRequest methodGet [] "/todos?status=done" "")
        >>= assertStatus 200

    (testRequest $ TestRequest methodGet [] "/todos?progress=finished" "")
        >>= assertStatus 404

    (testRequest $ TestRequest methodGet [] "/what" "")
        >>= assertStatus 404

    (testRequest $ TestRequest methodGet [] "/a" "")
        >>= assertStatus 200

main :: IO ()
main = Okapi.Test.runSession testSession liftIO testServer
