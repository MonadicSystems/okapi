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
    seg,
    segs,
    segParam,
    segWith,
    path,
    -- QUERY PARAM PARSERS
    queryParam,
    queryFlag,
    -- HEADER PARSERS
    header,
    auth,
    basicAuth,
    -- BODY PARSERS
    bodyJSON,
    bodyForm,
    -- RESPOND FUNCTIONS
    okPlainText,
    okJSON,
    ok,
    okLucid,
    connectEventSource,
    noContent,
    -- FAILURE FUNCTIONS
    skip,
    error,
    error500,
    error401,
    error403,
    error404,
    error422,
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
    Result (..),
    State (..),
  )
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web
import Prelude hiding (error, head)

-- FOR RUNNING OKAPI

runOkapi :: Monad m => (forall a. m a -> IO a) -> Int -> OkapiT m Result -> IO ()
runOkapi hoister port okapiT = do
  print $ "Running Okapi App on port " <> show port
  Warp.run port $ makeOkapiApp hoister okapiT

runOkapiTLS :: Monad m => (forall a. m a -> IO a) -> Warp.TLSSettings -> Warp.Settings -> OkapiT m Result -> IO ()
runOkapiTLS hoister tlsSettings settings okapiT = do
  print "Running servo on port 43"
  Warp.runTLS tlsSettings settings $ makeOkapiApp hoister okapiT

makeOkapiApp :: Monad m => (forall a. m a -> IO a) -> OkapiT m Result -> Wai.Application
makeOkapiApp hoister okapiT waiRequest respond = do
  -- eventSourcePoolTVar <- TVar.newTVarIO EventSource.emptyEventSourcePool
  (eitherFailureOrResult, _state) <- (StateT.runStateT . ExceptT.runExceptT . unOkapiT $ Morph.hoist hoister okapiT) (waiRequestToState {-eventSourcePoolTVar-} waiRequest)
  case eitherFailureOrResult of
    Left Skip -> respond $ Wai.responseLBS HTTP.status404 [] "Not Found"
    Left (Error response) -> respond . responseToWaiResponse $ response
    Right (ResultResponse response) -> respond . responseToWaiResponse $ response
    Right (ResultEventSource eventSource) -> (gzip def $ EventSource.eventSourceAppUnagiChan eventSource) waiRequest respond

-- Right (ResultJob job) -> (\_ _ -> do Concurrent.forkIO job; pure Wai.ResponseReceived)

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

{-
waiRequestToState :: TVar.TVar EventSource.EventSourcePool -> Wai.Request -> State
waiRequestToState stateEventSourcePoolTVar waiRequest =
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
-}

responseToWaiResponse :: Response -> Wai.Response
responseToWaiResponse Response {..} = Wai.responseLBS (toEnum $ fromEnum responseStatus) responseHeaders responseBody

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
  IO.liftIO $ print $ "Attempting to parse method: " <> Text.decodeUtf8 method
  state <- State.get
  logic state
  where
    logic :: State -> m ()
    logic state
      | isMethodParsed state = Except.throwError Skip
      | not $ methodMatches state method = Except.throwError Skip
      | otherwise = do
        IO.liftIO $ print $ "Method parsed: " <> Text.decodeUtf8 method
        State.put $ methodParsed state
        pure ()

-- PARSING PATHS

-- | Parses a single path segment matching the given text and discards it
seg :: forall m. MonadOkapi m => Text.Text -> m ()
seg goal = segWith (goal ==)

-- | Parses mutiple segments matching the order of the given list and discards them
-- | TODO: Needs testing. May not have the correct behavior
segs :: forall m. MonadOkapi m => [Text.Text] -> m ()
segs = mapM_ seg

segWith :: forall m. MonadOkapi m => (Text.Text -> Bool) -> m ()
segWith predicate = do
  IO.liftIO $ print "Attempting to parse seg"
  state <- State.get
  logic state
  where
    logic :: State -> m ()
    logic state
      | not $ segMatches state predicate = do
        IO.liftIO $ print "Couldn't match seg"
        Except.throwError Skip
      | otherwise = do
        IO.liftIO $ print $ "Path parsed: " <> show (getSeg state)
        State.put $ segParsed state
        pure ()

-- | TODO: Change Read a constraint to custom typeclass or FromHTTPApiData
-- | Parses a single seg segment, and returns the parsed seg segment as a value of the given type
segParam :: forall a m. (MonadOkapi m, Web.FromHttpApiData a) => m a
segParam = do
  IO.liftIO $ print "Attempting to get param from seg"
  state <- State.get
  logic state
  where
    logic :: State -> m a
    logic state =
      case getSeg state >>= Web.parseUrlPieceMaybe of
        Nothing -> Except.throwError Skip
        Just value -> do
          IO.liftIO $ print "Path param parsed"
          State.put $ segParsed state
          pure value

-- | Matches entire remaining path or fails
path :: forall m. MonadOkapi m => [Text.Text] -> m ()
path pathMatch = do
  state <- State.get
  logic state
  where
    logic :: State -> m ()
    logic state
      | getPath state /= pathMatch = Except.throwError Skip
      | otherwise = do
        State.put $ pathParsed state
        pure ()

-- PARSING QUERY PARAMETERS

-- | Parses a query parameter with the given name and returns the value as the given type
queryParam :: forall a m. (MonadOkapi m, Web.FromHttpApiData a) => Text.Text -> m a
queryParam key = do
  IO.liftIO $ print $ "Attempting to get query param " <> key
  state <- State.get
  logic state
  where
    logic :: State -> m a
    logic state
      | not $ isMethodParsed state = Except.throwError Skip
      | not $ isPathParsed state = Except.throwError Skip
      | otherwise =
        case getQueryItem state (key ==) of
          Nothing -> Except.throwError Skip
          Just queryItem -> case queryItem of
            (_, Nothing) ->
              Except.throwError Skip
            (_, Just param) -> case Web.parseQueryParamMaybe param of
              Nothing ->
                Except.throwError Skip
              Just value -> do
                IO.liftIO $ print $ "Query param parsed: " <> "(" <> key <> "," <> param <> ")"
                State.put $ queryParamParsed state queryItem
                pure value

queryFlag :: forall m. MonadOkapi m => Text.Text -> m Bool
queryFlag key = do
  IO.liftIO $ print $ "Checking if query param exists " <> key
  state <- State.get
  logic state
  where
    logic :: State -> m Bool
    logic state
      | not $ isMethodParsed state = Except.throwError Skip
      | not $ isPathParsed state = Except.throwError Skip
      | otherwise =
        case getQueryItem state (key ==) of
          Nothing -> pure False
          Just queryItem -> do
            IO.liftIO $ print $ "Query param exists: " <> key
            State.put $ queryParamParsed state queryItem
            pure True

-- PARSING HEADERS

header :: forall m. MonadOkapi m => HTTP.HeaderName -> m Text.Text
header headerName = do
  state <- State.get
  logic state
  where
    logic :: State -> m Text.Text
    logic state =
      case getHeader state headerName of
        Nothing -> Except.throwError Skip
        Just header@(name, value) -> pure $ Text.decodeUtf8 value

auth :: forall m. MonadOkapi m => m Text.Text
auth = header "Authorization"

basicAuth :: forall m. MonadOkapi m => m (Text.Text, Text.Text)
basicAuth = do
  IO.liftIO $ print "Attempting to get basic auth from headers"
  state <- State.get
  logic state
  where
    logic :: State -> m (Text.Text, Text.Text)
    logic state = do
      case getHeader state "Authorization" of
        Nothing -> Except.throwError Skip
        Just header@(_, authValue) -> do
          case Char8.words authValue of
            ["Basic", encodedCreds] -> case Base64.decodeBase64 encodedCreds of
              Left _ -> Except.throwError Skip
              Right decodedCreds -> case Char8.split ':' decodedCreds of
                [userID, password] -> do
                  IO.liftIO $ print "Basic auth acquired"
                  State.put $ headerParsed state header
                  pure $ Bifunctor.bimap Text.decodeUtf8 Text.decodeUtf8 (userID, password)
                _ -> Except.throwError Skip
            _ -> Except.throwError Skip

-- PARSING BODY

-- TODO: Check HEADERS for correct content type?
-- TODO: Check METHOD for correct HTTP method?

bodyJSON :: forall a m. (MonadOkapi m, Aeson.FromJSON a) => m a
bodyJSON = do
  IO.liftIO $ print "Attempting to parse JSON body"
  state <- State.get
  logic state
  where
    logic :: State -> m a
    logic state
      | not $ isMethodParsed state = Except.throwError Skip
      | not $ isPathParsed state = Except.throwError Skip
      | otherwise =
        do
          body <- IO.liftIO $ getRequestBody state
          case Aeson.decode body of
            Nothing -> do
              IO.liftIO $ print $ "Couldn't parse " <> show body
              Except.throwError Skip
            Just value -> do
              IO.liftIO $ print "JSON body parsed"
              State.put $ bodyParsed state
              pure value

bodyForm :: forall a m. (MonadOkapi m, Web.FromForm a) => m a
bodyForm = do
  IO.liftIO $ print "Attempting to parse FormURLEncoded body"
  state <- State.get
  logic state
  where
    logic :: State -> m a
    logic state
      | not $ isMethodParsed state = Except.throwError Skip
      | not $ isPathParsed state = Except.throwError Skip
      | otherwise =
        do
          body <- IO.liftIO $ getRequestBody state
          case eitherToMaybe $ Web.urlDecodeAsForm body of
            Nothing -> Except.throwError Skip
            Just value -> do
              IO.liftIO $ print "FormURLEncoded body parsed"
              State.put $ bodyParsed state
              pure value

-- RESPONSE FUNCTIONS

okPlainText :: forall m. MonadOkapi m => Headers -> Text.Text -> m Result
okPlainText headers = respond 200 ([("Content-Type", "text/plain")] <> headers) . LazyByteString.fromStrict . Text.encodeUtf8

okJSON :: forall a m. (MonadOkapi m, Aeson.ToJSON a) => Headers -> a -> m Result
okJSON headers = respond 200 ([("Content-Type", "application/json")] <> headers) . Aeson.encode

okLucid :: forall a m. (MonadOkapi m, Lucid.ToHtml a) => Headers -> a -> m Result
okLucid headers = ok headers . Lucid.renderBS . Lucid.toHtml

noContent :: forall a m. MonadOkapi m => Headers -> m Result
noContent headers = respond 204 headers ""

-- TODO: Use response builder
ok :: forall m. MonadOkapi m => Headers -> LazyByteString.ByteString -> m Result
ok headers = respond 200 ([("Content-Type", "text/html")] <> headers)

connectEventSource :: forall m. MonadOkapi m => EventSource.EventSource -> m Result
connectEventSource eventSource = do
  IO.liftIO $ print "Attempting to connect SSE source from Servo"
  state <- State.get
  logic state
  where
    logic :: State -> m Result
    logic state
      | not $ isMethodParsed state = Except.throwError Skip
      | not $ isPathParsed state = Except.throwError Skip
      | not $ isQueryParamsParsed state = Except.throwError Skip
      -- not $ isBodyParsed request = Except.throwError Skip
      | otherwise = do
        IO.liftIO $ print "Responded from servo, passing off to WAI"
        pure $ ResultEventSource eventSource

respond :: forall m. MonadOkapi m => Natural.Natural -> Headers -> LazyByteString.ByteString -> m Result
respond status headers body = do
  IO.liftIO $ print "Attempting to respond from Servo"
  state <- State.get
  logic state
  where
    logic :: State -> m Result
    logic state
      | not $ isMethodParsed state = Except.throwError Skip
      | not $ isPathParsed state = Except.throwError Skip
      | not $ isQueryParamsParsed state = Except.throwError Skip
      -- not $ isBodyParsed request = Except.throwError Skip
      | otherwise = do
        IO.liftIO $ print "Responded from servo, passing off to WAI"
        pure $ ResultResponse $ Response status headers body

-- ERROR FUNCTIONS

skip :: forall a m. MonadOkapi m => m a
skip = Except.throwError Skip

error :: forall a m. MonadOkapi m => Natural.Natural -> Headers -> LazyByteString.ByteString -> m a
error status headers = Except.throwError . Error . Response status headers

ok200 :: MonadOkapi m => Headers -> LazyByteString.ByteString -> m Result
ok200 = respond 200

error500 :: forall a m. MonadOkapi m => Headers -> LazyByteString.ByteString -> m a
error500 = error 500

error401 :: forall a m. MonadOkapi m => Headers -> LazyByteString.ByteString -> m a
error401 = error 401

error403 :: forall a m. MonadOkapi m => Headers -> LazyByteString.ByteString -> m a
error403 = error 403

error404 :: forall a m. MonadOkapi m => Headers -> LazyByteString.ByteString -> m a
error404 = error 404

error422 :: forall a m. MonadOkapi m => Headers -> LazyByteString.ByteString -> m a
error422 = error 422

-- | Execute the next parser even if the first one throws an Error error
(<!>) :: Monad m => OkapiT m a -> OkapiT m a -> OkapiT m a
(OkapiT (ExceptT.ExceptT (StateT.StateT mx))) <!> (OkapiT (ExceptT.ExceptT (StateT.StateT my))) = OkapiT . ExceptT.ExceptT . StateT.StateT $ \s -> do
  (eitherX, stateX) <- mx s
  case eitherX of
    Left Skip -> do
      (eitherY, stateY) <- my s
      case eitherY of
        Left Skip -> pure (Left Skip, s)
        Left error@(Error _) -> pure (Left error, s)
        Right y -> pure (Right y, stateY)
    Left error@(Error _) -> do
      (eitherY, stateY) <- my s
      case eitherY of
        Left Skip -> pure (Left Skip, s)
        Left error@(Error _) -> pure (Left error, s)
        Right y -> pure (Right y, stateY)
    Right x -> pure (Right x, stateX)

optionalError :: Monad m => OkapiT m a -> OkapiT m (Maybe a)
optionalError parser = (Just <$> parser) <!> pure Nothing

optionError :: Monad m => a -> OkapiT m a -> OkapiT m a
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