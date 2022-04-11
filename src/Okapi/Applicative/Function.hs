{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Okapi.Applicative.Function where

import Control.Applicative
import Control.Monad
import Control.Monad.Except (MonadError (..))
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (Identity)
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
import Data.OpenApi hiding (Response)
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
import Okapi.Applicative.Type
import Okapi.Monad.Function
import Okapi.Monad.Type
import Text.Read (readMaybe)
import Web.FormUrlEncoded (FromForm (fromForm), urlDecodeAsForm)
import Web.HttpApiData (FromHttpApiData)
import qualified Web.HttpApiData as HTTP

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

-- skipP :: Parser a
-- skipP = SkipP

mkAbortP :: Int -> [HTTP.Header] -> LBS.ByteString -> Parser Error
mkAbortP = MkAbortP

mkOkJSONP :: forall a. ToJSON a => [HTTP.Header] -> a -> Parser ResponseToken
mkOkJSONP = MkOkJSONP

mkOkHTMLP :: [HTTP.Header] -> LBS.ByteString -> Parser ResponseToken
mkOkHTMLP = MkOkHTMLP

mkOkPlainTextP :: [HTTP.Header] -> Text -> Parser ResponseToken
mkOkPlainTextP = MkOkPlainTextP

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
genServer (MkAbortP status headers body) = mkAbort status headers body
genServer (MkOkPlainTextP headers text) = mkOkPlainText headers text
genServer (MkOkHTMLP headers html) = mkOkHTML headers html
genServer (MkOkJSONP headers json) = mkOkJSON headers json
genServer (RespondP token) = respond token
genServer (ErrorP error) = throwError error
genServer (PureP v) = pure v
genServer (ApP f x) = genServer f <*> genServer x
genServer (ChoiceP f g) = genServer f <|> genServer g
genServer (BindP x f) = genServer (x >>= f)
