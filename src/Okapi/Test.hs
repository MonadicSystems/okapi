{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Test where

import qualified Control.Monad.Except as ExceptT
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.State.Strict as StateT
import Data.ByteString.Internal as BS
import Data.ByteString.Lazy.Internal as LBS
import Data.Function
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Network.Wai.Test (setRawPathInfo)
import qualified Network.Wai.Test as Wai.Test
import Okapi
import Okapi.State hiding (Request)

runSession ::
  Monad m =>
  Wai.Test.Session a ->
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  IO a
runSession session hoister okapiT = do
  let app = makeOkapiApp hoister notFound okapiT
  Wai.Test.runSession session app

withSession ::
  Monad m =>
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  Wai.Test.Session a ->
  IO a
withSession hoister okapiT session = runSession session hoister okapiT

data TestRequest = TestRequest
  { testRequestMethod :: HTTP.Method,
    testRequestHeaders :: HTTP.RequestHeaders,
    testRequestRawPath :: BS.ByteString,
    testRequestBody :: LBS.ByteString
  }

send :: TestRequest -> Wai.Test.Session Wai.Test.SResponse
send TestRequest {..} =
  let request =
        Wai.defaultRequest
          { Wai.requestMethod = testRequestMethod,
            Wai.requestHeaders = testRequestHeaders
          }
      simpleRequest = Wai.Test.setPath request testRequestRawPath
      simpleRequestBody = testRequestBody
      finalRequest = Wai.Test.SRequest simpleRequest simpleRequestBody
   in Wai.Test.srequest finalRequest

testParser ::
  Monad m =>
  (forall a. m a -> IO a) ->
  OkapiT m Response ->
  TestRequest ->
  IO (Either Failure Response, State)
testParser hoister okapiT testRequest =
  (StateT.runStateT . ExceptT.runExceptT . unOkapiT $ Morph.hoist hoister okapiT)
    (testRequestToState testRequest)

testRequestToState :: TestRequest -> State
testRequestToState = undefined

testRequestToSRequest :: TestRequest -> Wai.Test.SRequest
testRequestToSRequest testRequest = undefined
