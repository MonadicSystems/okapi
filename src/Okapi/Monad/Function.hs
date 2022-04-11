{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Monad.Function
  -- ( -- FOR RUNNING OKAPI
  --   runOkapi,
  --   runOkapiTLS,
  --   makeOkapiApp,
  --   -- METHOD PARSERS
  --   get,
  --   post,
  --   head,
  --   put,
  --   delete,
  --   trace,
  --   connect,
  --   options,
  --   patch,
  --   -- PATH PARSERS
  --   seg,
  --   segs,
  --   segParam,
  --   segWith,
  --   segParamAs,
  --   path,
  --   -- QUERY PARAM PARSERS
  --   queryParam,
  --   queryParamAs,
  --   queryParamFlag,
  --   -- HEADER PARSERS
  --   header,
  --   auth,
  --   basicAuth,
  --   -- BODY PARSERS
  --   bodyJSON,
  --   bodyForm,
  --   -- RESPOND FUNCTIONS
  --   okPlainText,
  --   respondJSON,
  --   respondJSONAp,
  --   respondHTML,
  --   -- ERROR FUNCTIONS
  --   skip,
  --   abort,
  --   abort500,
  --   abort401,
  --   abort403,
  --   abort404,
  --   abort422,
  --   (<!>),
  --   optionalAbort,
  --   optionAbort,
  -- )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Except (MonadError (..))
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Morph
import Control.Monad.RWS (MonadReader (local), join)
import Control.Monad.Reader.Class (MonadReader (ask, reader))
import qualified Control.Monad.State.Class as State
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.State (StateT (..))
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Aeson.Encoding (value)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import qualified Data.List as List
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types (status401)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (Settings)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WarpTLS (TLSSettings)
import qualified Network.Wai.Handler.WarpTLS as Warp
import Okapi.Monad.Type
import Text.Read (readMaybe)
import Web.FormUrlEncoded (FromForm (fromForm), urlDecodeAsForm)
import qualified Web.HttpApiData as HTTP
import Prelude hiding (head)
import Formatting
import Control.Monad.Log
import Data.Text.Lazy.Builder

-- FOR RUNNING OKAPI

runOkapi :: Monad m => (forall a. m a -> IO a) -> Int -> ServerT m Response -> IO ()
runOkapi hoister port okapiT = do
  print $ "Running Okapi App on port " <> show port
  Warp.run port $ makeOkapiApp hoister okapiT

runOkapiTLS :: Monad m => (forall a. m a -> IO a) -> TLSSettings -> Settings -> ServerT m Response -> IO ()
runOkapiTLS hoister tlsSettings settings okapiT = do
  print "Running servo on port 43"
  Warp.runTLS tlsSettings settings $ makeOkapiApp hoister okapiT

makeOkapiApp :: Monad m => (forall a. m a -> IO a) -> ServerT m Response -> Wai.Application
makeOkapiApp hoister okapiT request respond = do
  (eitherErrorsOrResponse, state) <- (runStateT . runExceptT . unOkapiT $ hoist hoister okapiT) (False, False, request)
  case eitherErrorsOrResponse of
    Left Skip -> respond $ Wai.responseLBS HTTP.status404 [] ""
    Left (Abort errResponse) -> respond $ responseToWaiResponse errResponse
    Right response -> respond $ responseToWaiResponse response

responseToWaiResponse :: Response -> Wai.Response
responseToWaiResponse Response{..} = Wai.responseLBS (toEnum responseStatus) responseHeaders responseBody

-- PARSING METHODS

get :: forall m. MonadServer m => m ()
get = method HTTP.methodGet

post :: forall m. MonadServer m => m ()
post = method HTTP.methodPost

head :: forall m. MonadServer m => m ()
head = method HTTP.methodHead

put :: forall m. MonadServer m => m ()
put = method HTTP.methodPut

delete :: forall m. MonadServer m => m ()
delete = method HTTP.methodDelete

trace :: forall m. MonadServer m => m ()
trace = method HTTP.methodTrace

connect :: forall m. MonadServer m => m ()
connect = method HTTP.methodConnect

options :: forall m. MonadServer m => m ()
options = method HTTP.methodOptions

patch :: forall m. MonadServer m => m ()
patch = method HTTP.methodPatch

method :: forall m. MonadServer m => HTTP.Method -> m ()
method method = do
  liftIO $ print $ "Attempting to parse method: " <> decodeUtf8 method
  request <- State.get
  logic request
  where
    logic :: Request -> m ()
    logic request
      | isMethodParsed request = throwError Skip
      | not $ methodMatches request method = throwError Skip
      | otherwise = do
        liftIO $ print $ "Method parsed: " <> decodeUtf8 method
        State.put $ methodParsed request
        pure ()

-- PARSING PATHS

-- | Parses a single path segment matching the given text and discards it
seg :: forall m. MonadServer m => Text -> m ()
seg goal = do
  liftIO $ print "Attempting to parse seg"
  request <- State.get
  logic request
  where
    logic :: Request -> m ()
    logic request
      | not $ segMatches request (goal ==) = do
        liftIO $ print "Couldn't match seg"
        throwError Skip
      | otherwise = do
        liftIO $ print $ "Path parsed: " <> show (getSeg request)
        State.put $ segParsed request
        pure ()

-- | Parses mutiple segments matching the order of the given list and discards them
-- | TODO: Needs testing. May not have the correct behavior
segs :: forall m. MonadServer m => [Text] -> m ()
segs = mapM_ seg

-- | Parses a single seg segment, and returns the parsed seg segment
segParam :: forall m. MonadServer m => m Text
segParam = do
  liftIO $ print "Attempting to get param from seg"
  request <- State.get
  logic request
  where
    logic :: Request -> m Text
    logic request =
      case getSeg request of
        Nothing -> throwError Skip
        Just seg -> do
          liftIO $ print $ "Path param parsed: " <> seg
          State.put $ segParsed request
          pure seg

segWith :: forall m. MonadServer m => (Text -> Bool) -> m ()
segWith predicate = do
  liftIO $ print "Attempting to parse seg"
  request <- State.get
  logic request
  where
    logic :: Request -> m ()
    logic request
      | not $ segMatches request predicate = do
        liftIO $ print "Couldn't match seg"
        throwError Skip
      | otherwise = do
        liftIO $ print $ "Path parsed: " <> show (getSeg request)
        State.put $ segParsed request
        pure ()

-- | TODO: Change Read a constraint to custom typeclass or FromHTTPApiData
-- | Parses a single seg segment, and returns the parsed seg segment as a value of the given type
segParamAs :: forall a m. (MonadServer m, Read a) => m a
segParamAs = do
  liftIO $ print "Attempting to get param from seg"
  request <- State.get
  logic request
  where
    logic :: Request -> m a
    logic request =
      case getSeg request >>= readTextMaybe of
        Nothing -> throwError Skip
        Just value -> do
          liftIO $ print "Path param parsed"
          State.put $ segParsed request
          pure value

-- | Matches entire remaining path or fails
path :: forall m. MonadServer m => [Text] -> m ()
path pathMatch = do
  request <- State.get
  logic request
  where
    logic :: Request -> m ()
    logic request
      | getPath request /= pathMatch = throwError Skip
      | otherwise = do
        State.put $ pathParsed request
        pure ()

-- PARSING QUERY PARAMETERS

-- | Parses a query parameter with the given name and returns the value as Text
queryParam :: forall m. MonadServer m => Text -> m Text
queryParam key = do
  liftIO $ print $ "Attempting to get query param " <> key
  request <- State.get
  logic request
  where
    logic :: Request -> m Text
    logic request
      | not $ isMethodParsed request = throwError Skip
      | not $ isPathParsed request = throwError Skip
      | otherwise =
        case getQueryItem request (key ==) of
          Nothing -> throwError Skip
          Just queryItem -> case queryItem of
            (_, Nothing) ->
              throwError Skip
            (_, Just valueBS) -> do
              liftIO $ print $ "Query param parsed: " <> "(" <> key <> "," <> decodeUtf8 valueBS <> ")"
              State.put $ queryParamParsed request queryItem
              pure $ decodeUtf8 valueBS

-- | Parses a query parameter with the given name and returns the value as the given type
queryParamAs :: forall a m. (MonadServer m, Read a) => Text -> m a
queryParamAs key = do
  liftIO $ print $ "Attempting to get query param " <> key
  request <- State.get
  logic request
  where
    logic :: Request -> m a
    logic request
      | not $ isMethodParsed request = throwError Skip
      | not $ isPathParsed request = throwError Skip
      | otherwise =
        case getQueryItem request (key ==) of
          Nothing -> throwError Skip
          Just queryItem -> case queryItem of
            (_, Nothing) ->
              throwError Skip
            (_, Just valueBS) -> case readBSMaybe valueBS of
              Nothing ->
                throwError Skip
              Just value -> do
                liftIO $ print $ "Query param parsed: " <> "(" <> key <> "," <> decodeUtf8 valueBS <> ")"
                State.put $ queryParamParsed request queryItem
                pure value

queryParamFlag :: forall m. MonadServer m => Text -> m Bool
queryParamFlag key = do
  liftIO $ print $ "Checking if query param exists " <> key
  request <- State.get
  logic request
  where
    logic :: Request -> m Bool
    logic request
      | not $ isMethodParsed request = throwError Skip
      | not $ isPathParsed request = throwError Skip
      | otherwise =
        case getQueryItem request (key ==) of
          Nothing -> pure False
          Just queryItem -> do
            liftIO $ print $ "Query param exists: " <> key
            State.put $ queryParamParsed request queryItem
            pure True

-- PARSING HEADERS

header :: forall m. MonadServer m => HTTP.HeaderName -> m Text
header headerName = do
  request <- State.get
  logic request
  where
    logic :: Request -> m Text
    logic request =
      case getHeader request headerName of
        Nothing -> throwError Skip
        Just header@(name, value) -> pure $ decodeUtf8 value

auth :: forall m. MonadServer m => m Text
auth = header "Authorization"

basicAuth :: forall m. MonadServer m => m (Text, Text)
basicAuth = do
  liftIO $ print "Attempting to get basic auth from headers"
  request <- State.get
  logic request
  where
    logic :: Request -> m (Text, Text)
    logic request = do
      case getHeader request "Authorization" of
        Nothing -> throwError Skip
        Just header@(_, authValue) -> do
          case BS.words authValue of
            ["Basic", encodedCreds] -> case decodeBase64 encodedCreds of
              Left _ -> throwError Skip
              Right decodedCreds -> case BS.split ':' decodedCreds of
                [userID, password] -> do
                  liftIO $ print "Basic auth acquired"
                  State.put $ headerParsed request header
                  pure $ bimap decodeUtf8 decodeUtf8 (userID, password)
                _ -> throwError Skip
            _ -> throwError Skip

-- PARSING BODY

-- TODO: Check HEADERS for correct content type?
-- TODO: Check METHOD for correct HTTP method?

bodyJSON :: forall a m. (MonadServer m, FromJSON a) => m a
bodyJSON = do
  liftIO $ print "Attempting to parse JSON body"
  request <- State.get
  logic request
  where
    logic :: Request -> m a
    logic request
      | not $ isMethodParsed request = throwError Skip
      | not $ isPathParsed request = throwError Skip
      | otherwise =
        do
          body <- liftIO $ getRequestBody request
          case decode body of
            Nothing -> do
              liftIO $ print $ "Couldn't parse " <> show body
              throwError Skip
            Just value -> do
              liftIO $ print "JSON body parsed"
              State.put $ bodyParsed request
              pure value

bodyForm :: forall a m. (MonadServer m, FromForm a) => m a
bodyForm = do
  liftIO $ print "Attempting to parse FormURLEncoded body"
  request <- State.get
  logic request
  where
    logic :: Request -> m a
    logic request
      | not $ isMethodParsed request = throwError Skip
      | not $ isPathParsed request = throwError Skip
      | otherwise =
        do
          body <- liftIO $ getRequestBody request
          case eitherToMaybe $ urlDecodeAsForm body of
            Nothing -> throwError Skip
            Just value -> do
              liftIO $ print "FormURLEncoded body parsed"
              State.put $ bodyParsed request
              pure value

-- RESPONSE FUNCTIONS

mkOkPlainText :: forall m. MonadServer m => [HTTP.Header] -> Text -> m ResponseToken
mkOkPlainText headers body = pure $ ResponseToken ([("Content-Type", "text/plain")] <> headers) (encode body)

mkOkJSON :: forall a m. (MonadServer m, ToJSON a) => [HTTP.Header] -> a -> m ResponseToken
mkOkJSON headers body = pure $ ResponseToken ([("Content-Type", "application/json")] <> headers) (encode body)

mkOkHTML :: forall m. MonadServer m => [HTTP.Header] -> LBS.ByteString -> m ResponseToken
mkOkHTML headers body = pure $ ResponseToken ([("Content-Type", "text/html")] <> headers) body

respond :: forall m. MonadServer m => ResponseToken -> m Response
respond (ResponseToken headers body) = do
  liftIO $ print "Attempting to response from Servo"
  request <- State.get
  logic request
  where
    logic :: Request -> m Response
    logic request
      | not $ isMethodParsed request = throwError Skip
      | not $ isPathParsed request = throwError Skip
      | not $ isQueryParamsParsed request = throwError Skip
      -- not $ isBodyParsed request = throwError Skip
      | otherwise = do
        liftIO $ print "Responded from servo, passing off to WAI"
        pure $ Response 200 headers body

-- ERROR FUNCTIONS

skip :: forall a m. MonadServer m => m a
skip = throwError Skip

error :: forall a m. MonadServer m => Error -> m a
error abort@(Abort _) = throwError abort
error _ = skip

mkAbort :: forall a m. MonadServer m => Int -> [HTTP.Header] -> LBS.ByteString -> m Error
mkAbort status headers body = pure $ Abort $ Response status headers body 

-- abort500 :: forall a m. MonadServer m => [HTTP.Header] -> LBS.ByteString -> m a
-- abort500 headers = abort . Response 500 headers

-- abort401 :: forall a m. MonadServer m => [HTTP.Header] -> LBS.ByteString -> m a
-- abort401 headers = abort . Response 401 headers

-- abort403 :: forall a m. MonadServer m => [HTTP.Header] -> LBS.ByteString -> m a
-- abort403 headers = abort . Response 403 headers

-- abort404 :: forall a m. MonadServer m => [HTTP.Header] -> LBS.ByteString -> m a
-- abort404 headers = abort . Response 404 headers

-- abort422 :: forall a m. MonadServer m => [HTTP.Header] -> LBS.ByteString -> m a
-- abort422 headers = abort . Response 422 headers

-- | Execute the next parser even if the first one throws an Abort error
(<!>) :: Monad m => ServerT m a -> ServerT m a -> ServerT m a
(ServerT (ExceptT (StateT mx))) <!> (ServerT (ExceptT (StateT my))) = ServerT . ExceptT . StateT $ \s -> do
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

optionalAbort :: Monad m => ServerT m a -> ServerT m (Maybe a)
optionalAbort parser = (Just <$> parser) <!> pure Nothing

optionAbort :: Monad m => a -> ServerT m a -> ServerT m a
optionAbort value parser = do
  mbValue <- optionalAbort parser
  case mbValue of
    Nothing -> pure value
    Just value' -> pure value'

-- PARSING GUARDS AND SWITCHES

isMethodParsed :: Request -> Bool
isMethodParsed (parsed, _, _) = parsed

isPathParsed :: Request -> Bool
isPathParsed (_, _, waiRequest) = Prelude.null $ Wai.pathInfo waiRequest

isQueryParamsParsed :: Request -> Bool
isQueryParamsParsed (_, _, waiRequest) = Prelude.null $ Wai.queryString waiRequest

isBodyParsed :: Request -> Bool
isBodyParsed (_, parsed, _) = parsed

methodMatches :: Request -> HTTP.Method -> Bool
methodMatches request method = method == getMethod request

segMatches :: Request -> (Text -> Bool) -> Bool
segMatches request predicate =
  maybe False predicate $ getSeg request

getMethod :: Request -> HTTP.Method
getMethod (_, _, waiRequest) = Wai.requestMethod waiRequest

getSeg :: Request -> Maybe Text
getSeg (_, _, waiRequest) = safeHead $ Wai.pathInfo waiRequest

getPath :: Request -> [Text]
getPath (_, _, waiRequest) = Wai.pathInfo waiRequest

getQueryItem :: Request -> (Text -> Bool) -> Maybe HTTP.QueryItem
getQueryItem (_, _, waiRequest) predicate = find (\(key, _) -> predicate . decodeUtf8 $ key) (Wai.queryString waiRequest)

getHeader :: Request -> HTTP.HeaderName -> Maybe HTTP.Header
getHeader (_, _, waiRequest) key = find (\(key', _) -> key == key') (Wai.requestHeaders waiRequest)

getRequestBody :: Request -> IO LBS.ByteString
getRequestBody (_, _, waiRequest) = Wai.strictRequestBody waiRequest

methodParsed :: Request -> Request
methodParsed (False, bodyParsed, waiRequest) = (True, bodyParsed, waiRequest)
methodParsed request = request

segParsed :: Request -> Request
segParsed (methodParsed, bodyParsed, waiRequest) =
  (methodParsed, bodyParsed, waiRequest {Wai.pathInfo = Prelude.drop 1 $ Wai.pathInfo waiRequest})

pathParsed :: Request -> Request
pathParsed (methodParsed, bodyParsed, waiRequest) =
  (methodParsed, bodyParsed, waiRequest {Wai.pathInfo = []})

queryParamParsed :: Request -> HTTP.QueryItem -> Request
queryParamParsed (methodParsed, bodyParsed, waiRequest) queryItem =
  (methodParsed, bodyParsed, waiRequest {Wai.queryString = List.delete queryItem (Wai.queryString waiRequest)})

-- TODO: Don't List.delete header??
headerParsed :: Request -> HTTP.Header -> Request
headerParsed (methodParsed, bodyParsed, waiRequest) header =
  (methodParsed, bodyParsed, waiRequest {Wai.requestHeaders = List.delete header (Wai.requestHeaders waiRequest)})

bodyParsed :: Request -> Request
bodyParsed (methodParsed, False, waiRequest) =
  (methodParsed, True, waiRequest)
bodyParsed request = request

-- HELPERS

eitherToMaybe :: Either l r -> Maybe r
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

readTextMaybe :: forall a. Read a => Text -> Maybe a
readTextMaybe = eitherToMaybe . HTTP.readTextData

readBSMaybe :: forall a. Read a => ByteString -> Maybe a
readBSMaybe = readTextMaybe . decodeUtf8

lookupBy :: forall a b. (a -> Bool) -> [(a, b)] -> Maybe b
lookupBy _ [] = Nothing
lookupBy predicate ((x, y) : xys)
  | predicate x = Just y
  | otherwise = lookupBy predicate xys
