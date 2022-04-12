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
    okHTML,
    -- ERROR FUNCTIONS
    skip,
    abort,
    abort500,
    abort401,
    abort403,
    abort404,
    abort422,
    -- ERROR HANDLING
    (<!>),
    optionalAbort,
    optionAbort,
  )
where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.State as StateT
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
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import Okapi.Type
  ( Error (Abort, Skip),
    Headers,
    MonadOkapi,
    OkapiT (..),
    QueryItem,
    Request (..),
    Response (..),
  )
import qualified Web.FormUrlEncoded as Web
import qualified Web.HttpApiData as Web
import Prelude hiding (head)

-- FOR RUNNING OKAPI

runOkapi :: Monad m => (forall a. m a -> IO a) -> Int -> OkapiT m Response -> IO ()
runOkapi hoister port okapiT = do
  print $ "Running Okapi App on port " <> show port
  Warp.run port $ makeOkapiApp hoister okapiT

runOkapiTLS :: Monad m => (forall a. m a -> IO a) -> Warp.TLSSettings -> Warp.Settings -> OkapiT m Response -> IO ()
runOkapiTLS hoister tlsSettings settings okapiT = do
  print "Running servo on port 43"
  Warp.runTLS tlsSettings settings $ makeOkapiApp hoister okapiT

makeOkapiApp :: Monad m => (forall a. m a -> IO a) -> OkapiT m Response -> Wai.Application
makeOkapiApp hoister okapiT request respond = do
  (eitherErrorsOrResponse, state) <- (StateT.runStateT . ExceptT.runExceptT . unOkapiT $ Morph.hoist hoister okapiT) (waiRequestToRequest request)
  case eitherErrorsOrResponse of
    Left Skip -> respond $ Wai.responseLBS HTTP.status404 [] "Not Found"
    Left (Abort response) -> respond . responseToWaiResponse $ response
    Right response -> respond . responseToWaiResponse $ response

waiRequestToRequest :: Wai.Request -> Request
waiRequestToRequest waiRequest =
  let requestMethod = Wai.requestMethod waiRequest
      requestPath = Wai.pathInfo waiRequest
      requestQuery = HTTP.queryToQueryText $ Wai.queryString waiRequest
      requestBody = Wai.strictRequestBody waiRequest
      requestHeaders = Wai.requestHeaders waiRequest
      requestVault = Wai.vault waiRequest
      requestMethodParsed = False
      requestBodyParsed = False
   in Request {..}

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
  request <- State.get
  logic request
  where
    logic :: Request -> m ()
    logic request
      | isMethodParsed request = Except.throwError Skip
      | not $ methodMatches request method = Except.throwError Skip
      | otherwise = do
        IO.liftIO $ print $ "Method parsed: " <> Text.decodeUtf8 method
        State.put $ methodParsed request
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
  request <- State.get
  logic request
  where
    logic :: Request -> m ()
    logic request
      | not $ segMatches request predicate = do
        IO.liftIO $ print "Couldn't match seg"
        Except.throwError Skip
      | otherwise = do
        IO.liftIO $ print $ "Path parsed: " <> show (getSeg request)
        State.put $ segParsed request
        pure ()

-- | TODO: Change Read a constraint to custom typeclass or FromHTTPApiData
-- | Parses a single seg segment, and returns the parsed seg segment as a value of the given type
segParam :: forall a m. (MonadOkapi m, Web.FromHttpApiData a) => m a
segParam = do
  IO.liftIO $ print "Attempting to get param from seg"
  request <- State.get
  logic request
  where
    logic :: Request -> m a
    logic request =
      case getSeg request >>= Web.parseUrlPieceMaybe of
        Nothing -> Except.throwError Skip
        Just value -> do
          IO.liftIO $ print "Path param parsed"
          State.put $ segParsed request
          pure value

-- | Matches entire remaining path or fails
path :: forall m. MonadOkapi m => [Text.Text] -> m ()
path pathMatch = do
  request <- State.get
  logic request
  where
    logic :: Request -> m ()
    logic request
      | requestPath request /= pathMatch = Except.throwError Skip
      | otherwise = do
        State.put $ pathParsed request
        pure ()

-- PARSING QUERY PARAMETERS

-- | Parses a query parameter with the given name and returns the value as the given type
queryParam :: forall a m. (MonadOkapi m, Web.FromHttpApiData a) => Text.Text -> m a
queryParam key = do
  IO.liftIO $ print $ "Attempting to get query param " <> key
  request <- State.get
  logic request
  where
    logic :: Request -> m a
    logic request
      | not $ isMethodParsed request = Except.throwError Skip
      | not $ isPathParsed request = Except.throwError Skip
      | otherwise =
        case getQueryItem request (key ==) of
          Nothing -> Except.throwError Skip
          Just queryItem -> case queryItem of
            (_, Nothing) ->
              Except.throwError Skip
            (_, Just param) -> case Web.parseQueryParamMaybe param of
              Nothing ->
                Except.throwError Skip
              Just value -> do
                IO.liftIO $ print $ "Query param parsed: " <> "(" <> key <> "," <> param <> ")"
                State.put $ queryParamParsed request queryItem
                pure value

queryFlag :: forall m. MonadOkapi m => Text.Text -> m Bool
queryFlag key = do
  IO.liftIO $ print $ "Checking if query param exists " <> key
  request <- State.get
  logic request
  where
    logic :: Request -> m Bool
    logic request
      | not $ isMethodParsed request = Except.throwError Skip
      | not $ isPathParsed request = Except.throwError Skip
      | otherwise =
        case getQueryItem request (key ==) of
          Nothing -> pure False
          Just queryItem -> do
            IO.liftIO $ print $ "Query param exists: " <> key
            State.put $ queryParamParsed request queryItem
            pure True

-- PARSING HEADERS

header :: forall m. MonadOkapi m => HTTP.HeaderName -> m Text.Text
header headerName = do
  request <- State.get
  logic request
  where
    logic :: Request -> m Text.Text
    logic request =
      case getHeader request headerName of
        Nothing -> Except.throwError Skip
        Just header@(name, value) -> pure $ Text.decodeUtf8 value

auth :: forall m. MonadOkapi m => m Text.Text
auth = header "Authorization"

basicAuth :: forall m. MonadOkapi m => m (Text.Text, Text.Text)
basicAuth = do
  IO.liftIO $ print "Attempting to get basic auth from headers"
  request <- State.get
  logic request
  where
    logic :: Request -> m (Text.Text, Text.Text)
    logic request = do
      case getHeader request "Authorization" of
        Nothing -> Except.throwError Skip
        Just header@(_, authValue) -> do
          case Char8.words authValue of
            ["Basic", encodedCreds] -> case Base64.decodeBase64 encodedCreds of
              Left _ -> Except.throwError Skip
              Right decodedCreds -> case Char8.split ':' decodedCreds of
                [userID, password] -> do
                  IO.liftIO $ print "Basic auth acquired"
                  State.put $ headerParsed request header
                  pure $ Bifunctor.bimap Text.decodeUtf8 Text.decodeUtf8 (userID, password)
                _ -> Except.throwError Skip
            _ -> Except.throwError Skip

-- PARSING BODY

-- TODO: Check HEADERS for correct content type?
-- TODO: Check METHOD for correct HTTP method?

bodyJSON :: forall a m. (MonadOkapi m, Aeson.FromJSON a) => m a
bodyJSON = do
  IO.liftIO $ print "Attempting to parse JSON body"
  request <- State.get
  logic request
  where
    logic :: Request -> m a
    logic request
      | not $ isMethodParsed request = Except.throwError Skip
      | not $ isPathParsed request = Except.throwError Skip
      | otherwise =
        do
          body <- IO.liftIO $ getRequestBody request
          case Aeson.decode body of
            Nothing -> do
              IO.liftIO $ print $ "Couldn't parse " <> show body
              Except.throwError Skip
            Just value -> do
              IO.liftIO $ print "JSON body parsed"
              State.put $ bodyParsed request
              pure value

bodyForm :: forall a m. (MonadOkapi m, Web.FromForm a) => m a
bodyForm = do
  IO.liftIO $ print "Attempting to parse FormURLEncoded body"
  request <- State.get
  logic request
  where
    logic :: Request -> m a
    logic request
      | not $ isMethodParsed request = Except.throwError Skip
      | not $ isPathParsed request = Except.throwError Skip
      | otherwise =
        do
          body <- IO.liftIO $ getRequestBody request
          case eitherToMaybe $ Web.urlDecodeAsForm body of
            Nothing -> Except.throwError Skip
            Just value -> do
              IO.liftIO $ print "FormURLEncoded body parsed"
              State.put $ bodyParsed request
              pure value

-- RESPONSE FUNCTIONS

okPlainText :: forall m. MonadOkapi m => Headers -> Text.Text -> m Response
okPlainText headers = respond 200 ([("Content-Type", "text/plain")] <> headers) . LazyByteString.fromStrict . Text.encodeUtf8

okJSON :: forall a m. (MonadOkapi m, Aeson.ToJSON a) => Headers -> a -> m Response
okJSON headers = respond 200 ([("Content-Type", "application/json")] <> headers) . Aeson.encode

okHTML :: forall m. MonadOkapi m => Headers -> LazyByteString.ByteString -> m Response
okHTML headers = respond 200 ([("Content-Type", "text/html")] <> headers)

respond :: forall m. MonadOkapi m => Natural.Natural -> Headers -> LazyByteString.ByteString -> m Response
respond status headers body = do
  IO.liftIO $ print "Attempting to respond from Servo"
  request <- State.get
  logic request
  where
    logic :: Request -> m Response
    logic request
      | not $ isMethodParsed request = Except.throwError Skip
      | not $ isPathParsed request = Except.throwError Skip
      | not $ isQueryParamsParsed request = Except.throwError Skip
      -- not $ isBodyParsed request = Except.throwError Skip
      | otherwise = do
        IO.liftIO $ print "Responded from servo, passing off to WAI"
        pure $ Response status headers body

-- ERROR FUNCTIONS

skip :: forall a m. MonadOkapi m => m a
skip = Except.throwError Skip

abort :: forall a m. MonadOkapi m => Natural.Natural -> Headers -> LazyByteString.ByteString -> m a
abort status headers = Except.throwError . Abort . Response status headers

ok200 :: MonadOkapi m => Headers -> LazyByteString.ByteString -> m Response
ok200 = respond 200

abort500 :: forall a m. MonadOkapi m => Headers -> LazyByteString.ByteString -> m a
abort500 = abort 500

abort401 :: forall a m. MonadOkapi m => Headers -> LazyByteString.ByteString -> m a
abort401 = abort 401

abort403 :: forall a m. MonadOkapi m => Headers -> LazyByteString.ByteString -> m a
abort403 = abort 403

abort404 :: forall a m. MonadOkapi m => Headers -> LazyByteString.ByteString -> m a
abort404 = abort 404

abort422 :: forall a m. MonadOkapi m => Headers -> LazyByteString.ByteString -> m a
abort422 = abort 422

-- | Execute the next parser even if the first one throws an Abort error
(<!>) :: Monad m => OkapiT m a -> OkapiT m a -> OkapiT m a
(OkapiT (ExceptT.ExceptT (StateT.StateT mx))) <!> (OkapiT (ExceptT.ExceptT (StateT.StateT my))) = OkapiT . ExceptT.ExceptT . StateT.StateT $ \s -> do
  (eitherX, stateX) <- mx s
  case eitherX of
    Left Skip -> do
      (eitherY, stateY) <- my s
      case eitherY of
        Left Skip -> pure (Left Skip, s)
        Left abort@(Abort _) -> pure (Left abort, s)
        Right y -> pure (Right y, stateY)
    Left abort@(Abort _) -> do
      (eitherY, stateY) <- my s
      case eitherY of
        Left Skip -> pure (Left Skip, s)
        Left abort@(Abort _) -> pure (Left abort, s)
        Right y -> pure (Right y, stateY)
    Right x -> pure (Right x, stateX)

optionalAbort :: Monad m => OkapiT m a -> OkapiT m (Maybe a)
optionalAbort parser = (Just <$> parser) <!> pure Nothing

optionAbort :: Monad m => a -> OkapiT m a -> OkapiT m a
optionAbort value parser = do
  mbValue <- optionalAbort parser
  case mbValue of
    Nothing -> pure value
    Just value' -> pure value'

-- PARSING GUARDS AND SWITCHES

isMethodParsed :: Request -> Bool
isMethodParsed Request {..} = requestMethodParsed

isPathParsed :: Request -> Bool
isPathParsed Request {..} = Prelude.null requestPath

isQueryParamsParsed :: Request -> Bool
isQueryParamsParsed Request {..} = Prelude.null requestQuery

isBodyParsed :: Request -> Bool
isBodyParsed Request {..} = requestBodyParsed

methodMatches :: Request -> HTTP.Method -> Bool
methodMatches Request {..} method = method == requestMethod

segMatches :: Request -> (Text.Text -> Bool) -> Bool
segMatches request predicate =
  maybe False predicate $ getSeg request

getSeg :: Request -> Maybe Text.Text
getSeg Request {..} = safeHead requestPath

getQueryItem :: Request -> (Text.Text -> Bool) -> Maybe QueryItem
getQueryItem Request {..} predicate = Foldable.find (\(key, _) -> predicate key) requestQuery

getHeader :: Request -> HTTP.HeaderName -> Maybe HTTP.Header
getHeader Request {..} key = Foldable.find (\(key', _) -> key == key') requestHeaders

getRequestBody :: Request -> IO LazyByteString.ByteString
getRequestBody Request {..} = requestBody

methodParsed :: Request -> Request
methodParsed request = request {requestMethodParsed = True}

segParsed :: Request -> Request
segParsed request = request {requestPath = Prelude.drop 1 $ requestPath request}

pathParsed :: Request -> Request
pathParsed request = request {requestPath = []}

queryParamParsed :: Request -> QueryItem -> Request
queryParamParsed request queryItem = request {requestQuery = List.delete queryItem $ requestQuery request}

-- TODO: Don't List.delete header??
headerParsed :: Request -> HTTP.Header -> Request
headerParsed request header = request {requestHeaders = List.delete header $ requestHeaders request}

bodyParsed :: Request -> Request
bodyParsed request = request {requestBodyParsed = True}

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
