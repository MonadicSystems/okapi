{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi
  ( -- MODULES
    module Okapi.Synonym,
    module Okapi.Parser,
    module Okapi.Response,
    -- TYPES
    State,
    Request,
    ToSSE (..),
    Event (..),
    EventSource,
    -- FOR RUNNING OKAPI
    runOkapi,
    runOkapiTLS,
    makeOkapiApp,
    -- METHOD HELPERS
    method,
    get,
    post,
    head,
    put,
    delete,
    trace,
    connect,
    options,
    patch,
    anyMethod,
    -- PATH HELPERS
    pathSegWith,
    pathSeg,
    path,
    pathParam,
    -- QUERY HELPERS
    queryParam,
    queryFlag,
    -- HEADER HELPERS
    header,
    basicAuth,
    cookies,
    -- BODY HELPERS
    bodyRaw,
    bodyJSON,
    bodyForm,
    -- SERVER SIDE EVENT HELPERS
    newEventSource,
    sendEvent,
    sendValue,
  )
where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Monad (MonadPlus, guard, (>=>))
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Base64
import qualified GHC.Natural as Natural
import qualified Lucid
import qualified Network.HTTP.Types as HTTP
import Network.Wai (ResponseReceived)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Internal as Wai
import Network.Wai.Middleware.Gzip (def, gzip)
import Okapi.Event
import qualified Okapi.Event as Event
import Okapi.Parser
import Okapi.Response
import Okapi.State
import Okapi.Synonym
import qualified Web.Cookie as Cookie
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web
import Prelude hiding (error, head)

-- FOR RUNNING OKAPI

runOkapi :: Monad m => (forall a. m a -> IO a) -> Response -> Int -> OkapiT m Response -> IO ()
runOkapi hoister defaultResponse port okapiT = do
  print $ "Running Okapi App on port " <> show port
  Warp.run port $ makeOkapiApp hoister defaultResponse okapiT

runOkapiTLS :: Monad m => (forall a. m a -> IO a) -> Response -> Warp.TLSSettings -> Warp.Settings -> OkapiT m Response -> IO ()
runOkapiTLS hoister defaultResponse tlsSettings settings okapiT = do
  print "Running servo on port 43"
  Warp.runTLS tlsSettings settings $ makeOkapiApp hoister defaultResponse okapiT

makeOkapiApp :: Monad m => (forall a. m a -> IO a) -> Response -> OkapiT m Response -> Wai.Application
makeOkapiApp hoister defaultResponse okapiT waiRequest respond = do
  (eitherFailureOrResponse, _state) <- (StateT.runStateT . ExceptT.runExceptT . unOkapiT $ Morph.hoist hoister okapiT) (waiRequestToState waiRequest)
  let response =
        case eitherFailureOrResponse of
          Left Skip                  -> defaultResponse
          Left (Error errorResponse) -> errorResponse
          Right succesfulResponse    -> succesfulResponse
  responseToWaiApp response waiRequest respond
  where
    waiRequestToState :: Wai.Request -> State
    waiRequestToState waiRequest =
      let requestMethod = Wai.requestMethod waiRequest
          requestPath = Wai.pathInfo waiRequest
          requestQuery = HTTP.queryToQueryText $ Wai.queryString waiRequest
          requestBody = Wai.strictRequestBody waiRequest
          requestHeaders = Wai.requestHeaders waiRequest
          requestVault = Wai.vault waiRequest
          stateRequest = Request {..}
          stateRequestMethodParsed = False
          stateRequestBodyParsed = False
          stateResponded = False
       in State {..}

    responseToWaiApp :: Response -> Wai.Application
    responseToWaiApp (Response {..}) waiRequest respond = case responseBody of
      ResponseBodyRaw body                -> respond $ Wai.responseLBS (toEnum $ fromEnum responseStatus) responseHeaders body
      ResponseBodyFile filePath           -> respond $ Wai.responseFile (toEnum $ fromEnum responseStatus) responseHeaders filePath Nothing
      ResponseBodyEventSource eventSource -> (gzip def $ Event.eventSourceAppUnagiChan eventSource) waiRequest respond

-- METHOD HELPERS

get :: forall m. MonadOkapi m => m ()
get = method HTTP.methodGet

post :: forall m. MonadOkapi m => m ()
post = method HTTP.methodPost

head :: forall m. MonadOkapi m => m ()
head = method HTTP.methodHead

put :: forall m. MonadOkapi m => m ()
put = method HTTP.methodPut

delete :: forall m. MonadOkapi m => m ()
delete = method HTTP.methodDelete

trace :: forall m. MonadOkapi m => m ()
trace = method HTTP.methodTrace

connect :: forall m. MonadOkapi m => m ()
connect = method HTTP.methodConnect

options :: forall m. MonadOkapi m => m ()
options = method HTTP.methodOptions

patch :: forall m. MonadOkapi m => m ()
patch = method HTTP.methodPatch

anyMethod :: forall m. MonadOkapi m => m ()
anyMethod = parseMethod >> pure ()

method :: forall m. MonadOkapi m => HTTP.Method -> m ()
method method = do
  method' <- parseMethod
  if method == method'
    then pure ()
    else skip

-- PATH HELPERS

-- | Parses a single path segment matching the given text and discards it
pathSeg :: forall m. MonadOkapi m => Text.Text -> m ()
pathSeg goal = pathSegWith (goal ==)

-- | Parses mutiple segments matching the order of the given list and discards them
-- | TODO: Needs testing. May not have the correct behavior
path :: forall m. MonadOkapi m => [Text.Text] -> m ()
path = mapM_ pathSeg

-- | Parses a single seg segment, and returns the parsed seg segment as a value of the given type
pathParam :: forall a m. (MonadOkapi m, Web.FromHttpApiData a) => m a
pathParam = do
  pathSeg <- parsePathSeg
  maybe skip pure (Web.parseUrlPieceMaybe pathSeg)

pathParamRaw :: forall m. MonadOkapi m => m Text.Text
pathParamRaw = parsePathSeg

pathSegWith :: forall m. MonadOkapi m => (Text.Text -> Bool) -> m ()
pathSegWith predicate = do
  pathSeg <- parsePathSeg
  if predicate pathSeg
    then pure ()
    else skip

-- QUERY HELPERS

queryParam :: forall a m. (MonadOkapi m, Web.FromHttpApiData a) => Text.Text -> m a
queryParam queryItemName = do
  (_, queryItemValue) <- parseQueryItem queryItemName
  maybe skip pure (Web.parseQueryParamMaybe =<< queryItemValue)

queryParamRaw :: forall m. MonadOkapi m => Text.Text -> m Text.Text
queryParamRaw queryItemName = do
  (_, queryItemValue) <- parseQueryItem queryItemName
  maybe skip pure queryItemValue

queryFlag :: forall a m. MonadOkapi m => Text.Text -> m ()
queryFlag queryItemName = parseQueryItem queryItemName >> pure ()

-- HEADER HELPERS

basicAuth :: forall m. MonadOkapi m => m (Text.Text, Text.Text)
basicAuth = do
  authValue <- header "Authorization"
  case Text.words $ Text.decodeUtf8 authValue of
    ["Basic", encodedCreds] ->
      case decodeBase64 encodedCreds of
        Left _ -> skip
        Right decodedCreds ->
          case Text.split (== ':') decodedCreds of
            [userID, password] -> pure (userID, password)
            _ -> skip
    _ -> skip

cookies :: forall m. MonadOkapi m => m Cookies
cookies = do
  cookiesValue <- header "Cookie"
  pure $ Cookie.parseCookiesText cookiesValue

-- TODO: Any checks required??
header :: forall m. MonadOkapi m => HTTP.HeaderName -> m Char8.ByteString
header headerName = do
  (_headerName, headerValue) <- parseHeader headerName
  pure headerValue

-- BODY HELPERS

-- TODO: Check HEADERS for correct content type?
-- TODO: Check METHOD for correct HTTP method?

bodyJSON :: forall a m. (MonadOkapi m, Aeson.FromJSON a) => m a
bodyJSON = do
  body <- bodyRaw
  maybe skip pure (Aeson.decode body)

bodyForm :: forall a m. (MonadOkapi m, Web.FromForm a) => m a
bodyForm = do
  body <- bodyRaw
  maybe skip pure (eitherToMaybe $ Web.urlDecodeAsForm body)
  where
    eitherToMaybe :: Either l r -> Maybe r
    eitherToMaybe either = case either of
      Left _ -> Nothing
      Right value -> Just value

-- TODO: bodyFile functions for file uploads to server?
bodyRaw :: forall m. MonadOkapi m => m LazyByteString.ByteString
bodyRaw = parseBody
