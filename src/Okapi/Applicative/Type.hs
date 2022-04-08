{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Okapi.Applicative.Type where

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

-- type Request =
--   ( Bool, -- has the HTTP method been parsed yet?
--     Bool, -- has body been parsed yet? Needed this because of possibilty of large bodies and streaming bodies
--     Wai.Request
--   )

-- type Response = Wai.Response

-- data Error = Skip | Abort Response

-- newtype ParserT m a = ParserT {runParserT :: ExceptT Error (StateT Request m) a}

{-
instance Functor Parser where
  fmap f (Parser parser) = Parser $ \s -> case parser s of
    (Left error, _) -> (Left error, s)
    (Right x, s') -> (Right $ f x, s')

instance Applicative Parser where
  pure x = Parser $ \s -> (Right x, s)
  {-# INLINEABLE pure #-}
  (Parser parserF) <*> (Parser parserX) = Parser $ \s -> case parserF s of
    (Left error, _) -> (Left error, s)
    (Right f, s') -> case parserX s' of
      (Left error, _) -> (Left error, s')
      (Right x, s'') -> (Right $ f x, s'')
  {-# INLINEABLE (<*>) #-}

instance Alternative Parser where
  empty = Parser $ \s -> (Left Skip, s)
  {-# INLINE empty #-}
  (Parser parserA) <|> (Parser parserB) = Parser $ \s -> case parserA s of
    (Left Skip, _) -> case parserB s of
      (Left Skip, _) -> (Left Skip, s)
      (Left abort@(Abort _), _) -> (Left abort, s)
      (Right b, stateB) -> (Right b, stateB)
    (Left abort@(Abort _), _) -> (Left abort, s)
    (Right a, stateA) -> (Right a, stateA)
  {-# INLINEABLE (<|>) #-}

method :: HTTP.Method -> Parser ()
method = undefined
-}

-- parse :: MonadServer m => Parser a -> Request -> m a
-- parse parser = runM (genServer parser)

-- runM :: MonadServer m => m a -> Request -> m a
-- runM parserT = runStateT (runExceptT $ runParserT parserT)

{-
  MethodP :: HTTP.Method -> Parser ()
  SegP :: Text -> Parser ()
  SegParamP :: forall a. FromHttpApiData a => Parser a
  QueryParamP :: forall a. FromHttpApiData a => Text -> Parser a
  QueryFlagP :: Text -> Parser Bool
  HeaderP :: HTTP.HeaderName -> Parser ByteString
  BodyJSONP :: forall a. FromJSON a => Parser a
  BodyFormP :: forall a. FromForm a => Parser a
  SkipP :: Parser a
  -- AbortP :: Response -> Parser ()
  -- OkP :: Response -> Parser Response
  PureP :: a -> Parser a
  AppP :: Parser (a -> b) -> Parser a -> Parser b
  ChoiceP :: Parser a -> Parser a -> Parser a
-}

data Parser a where
  MethodP :: HTTP.Method -> Parser ()
  SegP :: Text -> Parser ()
  SegParamP :: forall a. Read a => Parser a
  QueryParamP :: forall a. Read a => Text -> Parser a
  QueryFlagP :: Text -> Parser Bool
  HeaderP :: HTTP.HeaderName -> Parser Text
  BodyJSONP :: forall a. FromJSON a => Parser a
  BodyFormP :: forall a. FromForm a => Parser a
  RespondP :: Response -> Parser Response
  RespondPlainTextP :: [HTTP.Header] -> Parser Text -> Parser Response
  RespondHTMLP :: [HTTP.Header] -> Parser LBS.ByteString -> Parser Response
  RespondJSONP :: forall a. ToJSON a => [HTTP.Header] -> Parser a -> Parser Response
  SkipP :: Parser a
  PureP :: a -> Parser a
  AppP :: Parser (a -> b) -> Parser a -> Parser b
  ChoiceP :: Parser a -> Parser a -> Parser a
  -- BindP :: Parser a -> (a -> Parser b) -> Parser b
  -- genServer (BindP x f) = genServer x >>= f

instance Functor Parser where
  fmap = AppP . PureP

instance Applicative Parser where
  pure = PureP
  (<*>) = AppP

instance Alternative Parser where
  empty = SkipP
  (<|>) = ChoiceP
