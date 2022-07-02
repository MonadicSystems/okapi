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

module Okapi.Function
  ( -- FOR RUNNING OKAPI
    runOkapi,
    runOkapiTLS,
    makeOkapiApp,
    -- METHOD PARSERS
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
    -- PATH PARSERS
    pathSegWith,
    pathSeg,
    path,
    pathParam,
    -- QUERY PARAM PARSERS
    queryParam,
    queryFlag,
    -- HEADER PARSERS
    header,
    basicAuth,
    -- BODY PARSERS
    bodyRaw,
    bodyJSON,
    bodyForm,
    -- RESPOND FUNCTIONS
    ok,
    okPlainText,
    okJSON,
    okHTML,
    okLucid,
    okEventSource,
    okFile,
    noContent,
    redirectTo,
    -- FAILURE FUNCTIONS
    skip,
    error,
    error401,
    error403,
    error404,
    error422,
    error500,
    -- GUARD FUNCTIONS
    guardError,
    guard401,
    guard403,
    guard422,
    guard500,
    -- ERROR HANDLING
    (<!>),
    optionalError,
    optionError,
  )
where

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM.TVar as TVar
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
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified GHC.Natural as Natural
import qualified Lucid
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Internal as Wai
import Network.Wai.Middleware.Gzip (gzip, def)
import qualified Okapi.EventSource as EventSource
import Okapi.Type
  ( Failure (Error, Skip),
    Headers,
    MonadOkapi,
    OkapiT (..),
    QueryItem,
    Request (..),
    Response (..),
    ResponseBody (..),
    State (..),
  )
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web
import Prelude hiding (error, head)
import Network.Wai (ResponseReceived)
import Data.Functor ((<&>))
import Control.Monad (guard)
import Data.Function ((&))
import Data.Text.Encoding.Base64

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
  (eitherFailureOrResponse, _state) <- (StateT.runStateT . ExceptT.runExceptT . unOkapiT $ Morph.hoist hoister okapiT) (waiRequestToState {-eventSourcePoolTVar-} waiRequest)
  let response =
        case eitherFailureOrResponse of
          Left Skip                  -> defaultResponse
          Left (Error errorResponse) -> errorResponse
          Right succesfulResponse    -> succesfulResponse
  responseToWaiApp response waiRequest respond

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
   in State {..}

responseToWaiApp :: Response -> Wai.Application
responseToWaiApp (Response {..}) waiRequest respond = case responseBody of
  ResponseBodyRaw body                -> respond $ Wai.responseLBS (toEnum $ fromEnum responseStatus) responseHeaders body
  ResponseBodyFile filePath           -> respond $ Wai.responseFile (toEnum $ fromEnum responseStatus) responseHeaders filePath Nothing
  ResponseBodyEventSource eventSource -> (gzip def $ EventSource.eventSourceAppUnagiChan eventSource) waiRequest respond

-- PARSING METHODS

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

method :: forall m. MonadOkapi m => HTTP.Method -> m ()
method method = do
  state <- State.get
  guard (not $ isMethodParsed state)
  guard (methodMatches state method)
  State.put $ methodParsed state

-- PARSING PATHS

pathSegWith :: forall m. MonadOkapi m => (Text.Text -> Bool) -> m ()
pathSegWith predicate = do
  state <- State.get
  guard (isMethodParsed state)
  guard (segMatches state predicate)
  State.put $ segParsed state

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
  state <- State.get
  guard (isMethodParsed state)
  case getSeg state >>= Web.parseUrlPieceMaybe of
    Nothing    -> skip
    Just value -> do
      State.put $ segParsed state
      pure value

-- PARSING QUERY PARAMETERS

-- | Parses a query parameter with the given name and returns the value as the given type
queryParam :: forall a m. (MonadOkapi m, Web.FromHttpApiData a) => Text.Text -> m a
queryParam key = do
  state <- State.get
  guard (isMethodParsed state)
  guard (isPathParsed state)
  case getQueryItem state (key ==) of
    Nothing -> skip
    Just queryItem -> case queryItem of
      (_, Nothing) -> skip
      (_, Just param) -> case Web.parseQueryParamMaybe param of
        Nothing -> skip
        Just value -> do
          State.put $ queryParamParsed state queryItem
          pure value

queryFlag :: forall m. MonadOkapi m => Text.Text -> m Bool
queryFlag key = do
  state <- State.get
  guard (isMethodParsed state)
  guard (isPathParsed state)
  case getQueryItem state (key ==) of
    Nothing -> pure False
    Just queryItem -> do
      State.put $ queryParamParsed state queryItem
      pure True

-- PARSING HEADERS

-- TODO: Any checks required??
header :: forall m. MonadOkapi m => HTTP.HeaderName -> m Text.Text
header headerName = do
  state <- State.get
  case getHeader state headerName of
    Nothing -> skip
    Just header@(name, value) -> do
      State.put $ headerParsed state header
      pure $ Text.decodeUtf8 value

basicAuth :: forall m. MonadOkapi m => m (Text.Text, Text.Text)
basicAuth = do
  IO.liftIO $ print "Attempting to get basic auth from headers"
  state <- State.get
  authValue <- header "Authorization"
  case Text.words authValue of
    ["Basic", encodedCreds] ->
      case decodeBase64 encodedCreds of
        Left _ -> skip
        Right decodedCreds ->
          case Text.split (== ':') decodedCreds of
            [userID, password] -> pure (userID, password)
            _ -> skip
    _ -> skip

-- PARSING BODY

-- TODO: Check HEADERS for correct content type?
-- TODO: Check METHOD for correct HTTP method?

bodyRaw :: forall m. MonadOkapi m => m LazyByteString.ByteString
bodyRaw = do
  state <- State.get
  guard (isMethodParsed state)
  guard (isPathParsed state)
  State.put $ bodyParsed state
  IO.liftIO $ getRequestBody state

bodyJSON :: forall a m. (MonadOkapi m, Aeson.FromJSON a) => m a
bodyJSON = do
  body <- bodyRaw
  maybe skip pure (Aeson.decode body)

bodyForm :: forall a m. (MonadOkapi m, Web.FromForm a) => m a
bodyForm = do
  body <- bodyRaw
  maybe skip pure (eitherToMaybe $ Web.urlDecodeAsForm body)

-- TODO: bodyFile functions for file uploads to server?

-- RESPONSE FUNCTIONS

-- TODO: Add Responded already to state so you can't respond twice
respond :: forall m. MonadOkapi m => Response -> m Response
respond response = do
  IO.liftIO $ print "Attempting to respond from Servo"
  state <- State.get
  logic state
  where
    logic :: State -> m Response
    logic state
      | not $ isMethodParsed state = Except.throwError Skip
      | not $ isPathParsed state = Except.throwError Skip
      | not $ isQueryParamsParsed state = Except.throwError Skip
      -- not $ isBodyParsed request = Except.throwError Skip
      | otherwise = do
        IO.liftIO $ print "Responded from servo, passing off to WAI"
        pure response

ok :: forall m. MonadOkapi m => m Response
ok =
  let
    responseStatus = 200
    responseHeaders = []
    responseBody = ResponseBodyRaw ""
  in respond Response {..}

noContent :: forall m. MonadOkapi m => m Response
noContent =
  let
    responseStatus = 204
    responseHeaders = []
    responseBody = ResponseBodyRaw ""
  in respond Response {..}

-- TODO: Change type of URL?
redirectTo :: forall m. MonadOkapi m => Char8.ByteString -> m Response
redirectTo url =
  let
    responseStatus = 302
    responseHeaders = [("Location", url)]
    responseBody = ResponseBodyRaw ""
  in respond Response {..}

setResponseStatus :: Natural.Natural -> Response -> Response
setResponseStatus status response = response { responseStatus = status }

setResponseHeaders :: Headers -> Response -> Response
setResponseHeaders headers response = response { responseHeaders = headers }

setResponseHeader :: HTTP.Header -> Response -> Response
setResponseHeader header response@Response{..} =
  response { responseHeaders = update header responseHeaders }

setResponseBody :: ResponseBody -> Response -> Response
setResponseBody body response = response { responseBody = body }

-- TODO: Use response builder?
okHTML :: forall m. MonadOkapi m => LazyByteString.ByteString -> m Response
okHTML html =
  ok <&>
  setResponseHeader ("Content-Type", "text/html") .
  setResponseBody (ResponseBodyRaw html)

okPlainText :: forall m. MonadOkapi m => Text.Text -> m Response
okPlainText text =
  let raw = LazyByteString.fromStrict . Text.encodeUtf8 $ text
  in
    ok <&>
    setResponseHeader ("Content-Type", "text/plain") .
    setResponseBody (ResponseBodyRaw raw)

okJSON :: forall m a. (MonadOkapi m, Aeson.ToJSON a) => a -> m Response
okJSON value =
  let raw = Aeson.encode value
  in
    ok <&>
    setResponseHeader ("Content-Type", "application/json") .
    setResponseBody (ResponseBodyRaw raw)

okLucid :: forall m a. (MonadOkapi m, Lucid.ToHtml a) => a -> m Response
okLucid value =
  let raw = Lucid.renderBS . Lucid.toHtml $ value
  in okHTML raw

-- TODO: setResponseHeaders to octet/binary or something?
okFile :: forall m. MonadOkapi m => FilePath -> m Response
okFile filePath =
  ok <&>
  setResponseBody (ResponseBodyFile filePath)

okEventSource :: forall m. MonadOkapi m => EventSource.EventSource -> m Response
okEventSource eventSource =
  ok <&>
  setResponseBody (ResponseBodyEventSource eventSource)

-- ERROR FUNCTIONS

skip :: forall a m. MonadOkapi m => m a
skip = Except.throwError Skip

error :: forall a m. MonadOkapi m => Natural.Natural -> Headers -> LazyByteString.ByteString -> m a
error status headers =
  Except.throwError .
  Error .
  Response status headers .
  ResponseBodyRaw

error401 :: forall a m. MonadOkapi m => LazyByteString.ByteString -> m a
error401 = error 401 []

error403 :: forall a m. MonadOkapi m => LazyByteString.ByteString -> m a
error403 = error 403 []

error404 :: forall a m. MonadOkapi m => LazyByteString.ByteString -> m a
error404 = error 404 []

error422 :: forall a m. MonadOkapi m => LazyByteString.ByteString -> m a
error422 = error 422 []

error500 :: forall a m. MonadOkapi m => LazyByteString.ByteString -> m a
error500 = error 500 []

-- GUARD FUNCTIONS

guardError :: forall a m. MonadOkapi m => Natural.Natural -> LazyByteString.ByteString -> Bool -> m ()
guardError _ _ True          = pure ()
guardError status body False = error status [] body

guard401 :: forall a m. MonadOkapi m => LazyByteString.ByteString -> Bool -> m ()
guard401 = guardError 401

guard403 :: forall a m. MonadOkapi m => LazyByteString.ByteString -> Bool -> m ()
guard403 = guardError 403

guard404 :: forall a m. MonadOkapi m => LazyByteString.ByteString -> Bool -> m ()
guard404 = guardError 404

guard422 :: forall a m. MonadOkapi m => LazyByteString.ByteString -> Bool -> m ()
guard422 = guardError 422

guard500 :: forall a m. MonadOkapi m => LazyByteString.ByteString -> Bool -> m ()
guard500 = guardError 500

-- | Execute the next parser even if the first one throws an Error error
(<!>) :: MonadOkapi m => m a -> m a -> m a
parser1 <!> parser2 = Except.catchError parser1 (const parser2)

optionalError :: MonadOkapi m => m a -> m (Maybe a)
optionalError parser = (Just <$> parser) <!> pure Nothing

optionError :: MonadOkapi m => a -> m a -> m a
optionError value parser = do
  mbValue <- optionalError parser
  case mbValue of
    Nothing -> pure value
    Just value' -> pure value'

-- PARSING GUARDS AND SWITCHES

isMethodParsed :: State -> Bool
isMethodParsed State {..} = stateRequestMethodParsed

isPathParsed :: State -> Bool
isPathParsed State {..} = Prelude.null $ requestPath stateRequest

isQueryParamsParsed :: State -> Bool
isQueryParamsParsed State {..} = Prelude.null $ requestQuery stateRequest

isBodyParsed :: State -> Bool
isBodyParsed State {..} = stateRequestBodyParsed

methodMatches :: State -> HTTP.Method -> Bool
methodMatches State {..} method = method == requestMethod stateRequest

segMatches :: State -> (Text.Text -> Bool) -> Bool
segMatches state predicate =
  maybe False predicate $ getSeg state

getPath :: State -> [Text.Text]
getPath State {..} = requestPath stateRequest

getSeg :: State -> Maybe Text.Text
getSeg State {..} = safeHead (requestPath stateRequest)

getQueryItem :: State -> (Text.Text -> Bool) -> Maybe QueryItem
getQueryItem State {..} predicate = Foldable.find (\(key, _) -> predicate key) (requestQuery stateRequest)

getHeader :: State -> HTTP.HeaderName -> Maybe HTTP.Header
getHeader State {..} key = Foldable.find (\(key', _) -> key == key') (requestHeaders stateRequest)

getRequestBody :: State -> IO LazyByteString.ByteString
getRequestBody State {..} = requestBody stateRequest

methodParsed :: State -> State
methodParsed state = state {stateRequestMethodParsed = True}

segParsed :: State -> State
segParsed state = state {stateRequest = (stateRequest state) {requestPath = Prelude.drop 1 $ requestPath $ stateRequest state}}

pathParsed :: State -> State
pathParsed state = state {stateRequest = (stateRequest state) {requestPath = []}}

queryParamParsed :: State -> QueryItem -> State
queryParamParsed state queryItem = state {stateRequest = (stateRequest state) {requestQuery = List.delete queryItem $ requestQuery $ stateRequest state}}

-- TODO: Don't List.delete header??
headerParsed :: State -> HTTP.Header -> State
headerParsed state header = state {stateRequest = (stateRequest state) {requestHeaders = List.delete header $ requestHeaders $ stateRequest state}}

bodyParsed :: State -> State
bodyParsed state = state {stateRequestBodyParsed = True}

-- HELPERS

eitherToMaybe :: Either l r -> Maybe r
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

lookupBy :: forall a b. (a -> Bool) -> [(a, b)] -> Maybe b
lookupBy _ [] = Nothing
lookupBy predicate ((x, y) : xys)
  | predicate x = Just y
  | otherwise = lookupBy predicate xys

update :: forall a b. Eq a => (a, b) -> [(a, b)] -> [(a, b)]
update pair [] = [pair]
update pair@(key, value) (pair'@(key', value'):ps) =
  if key == key'
    then pair : ps
    else pair' : update pair ps
