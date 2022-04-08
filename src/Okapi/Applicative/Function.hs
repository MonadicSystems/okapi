{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Okapi.Applicative.Function where

import Control.Applicative
import Control.Monad
import Control.Monad.Except (MonadError (..))
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Log
import Control.Monad.Morph
import Control.Monad.RWS (MonadReader (local), join)
import Control.Monad.Reader.Class (MonadReader (ask, reader))
import Control.Monad.State.Class (MonadState (..))
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
import Data.List (delete, deleteBy)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Formatting
import Network.HTTP.Types (status401)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (Settings)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WarpTLS (TLSSettings)
import qualified Network.Wai.Handler.WarpTLS as Warp
import Text.Read (readMaybe)
import Web.FormUrlEncoded (FromForm (fromForm), urlDecodeAsForm)
import Web.HttpApiData (FromHttpApiData)
import qualified Web.HttpApiData as HTTP
import Control.Monad.Identity (Identity)
import Okapi.Monad.Type
import Okapi.Monad.Function
import Data.OpenApi hiding (Response)
import Okapi.Applicative.Type

methodP :: HTTP.Method -> Parser ()
methodP = MethodP

getP :: Parser ()
getP = MethodP "GET"

segP :: Text -> Parser ()
segP = SegP

segParamP :: forall a. Read a => Parser a
segParamP = SegParamP

queryParamP :: forall a. Read a => Text -> Parser a
queryParamP = QueryParamP

queryFlagP :: Text -> Parser Bool
queryFlagP = QueryFlagP

headerP :: HTTP.HeaderName -> Parser Text
headerP = HeaderP

bodyJSONP :: forall a. FromJSON a => Parser a
bodyJSONP = BodyJSONP

bodyFormP :: forall a. FromForm a => Parser a
bodyFormP = BodyFormP

skipP :: Parser a
skipP = SkipP

respondP :: Response -> Parser Response
respondP = RespondP

-- abortP :: Response -> Parser a
-- abortP = AbortP

-- okP :: Response -> Parser Response
-- okP = OkP

-- okJSONP :: forall a. ToJSON a => [HTTP.Header] -> Parser a -> Parser Response
-- okJSONP = OkJSONP

-- okHTMLP :: [HTTP.Header] -> Parser LBS.ByteString -> Parser Response
-- okHTMLP = OkHTMLP

-- okPlainTextP :: [HTTP.Header] -> Parser Text -> Parser Response
-- okPlainTextP = OkPlainTextP

respondJSONP :: forall a. ToJSON a => [HTTP.Header] -> Parser a -> Parser Response
respondJSONP = RespondJSONP

-- okHTMLP :: [HTTP.Header] -> LBS.ByteString -> Parser Response
-- okHTMLP headers = OkHTMLP headers . pure

-- okPlainTextP :: [HTTP.Header] -> Text -> Parser Response
-- okPlainTextP headers = OkPlainTextP headers . pure

optimize :: Parser Response -> Parser Response
optimize = undefined

genOpenAPI :: Parser Response -> OpenApi
genOpenAPI = undefined

genLayout :: Parser Response -> Text
genLayout = undefined

genPath :: Parser Response -> Parser Response -> Text
genPath = undefined

genServer :: MonadIO m => Parser a -> ServerT m a
genServer (MethodP m) = method m
genServer (SegP s) = seg s
genServer SegParamP = segParamAs
genServer (QueryParamP name) = queryParamAs name
genServer (QueryFlagP name) = queryParamFlag name
genServer (HeaderP headerName) = header headerName
genServer BodyJSONP = bodyJSON
genServer BodyFormP = bodyForm
genServer SkipP = skip
genServer (RespondP response) = undefined
genServer (RespondPlainTextP headers text) = okPlainTextAp headers (genServer text)
genServer (RespondHTMLP headers html) = okHTMLAp headers (genServer html)
genServer (RespondJSONP headers json) = okJSONAp headers (genServer json)
genServer (PureP v) = pure v
genServer (AppP f x) = genServer f <*> genServer x
genServer (ChoiceP f g) = genServer f <|> genServer g
-- genServer (BindP x f) = genServer x >>= f
