{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Okapi.Grammar where

import Control.Applicative
import Control.Monad
import Control.Monad.Except (MonadError (..))
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (Identity)
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
import Okapi.Type

-- class Monad m => Okapi m where
--   method :: HTTP.Method -> m ()
--   seg :: Text -> m ()
--   segParam :: forall a. Read a => m a
--   queryParam :: forall a. Read a => Text -> m a
--   queryFlag :: Text -> m Bool
--   header :: HTTP.HeaderName -> m Text
--   bodyJSON :: forall a. FromJSON a => m a
--   bodyForm :: forall a. FromForm a => m a
--   mayRespondPlainText :: Int -> [HTTP.Header] -> Text -> m ResponseToken
--   mayRespondHTML :: Int -> [HTTP.Header] -> LBS.ByteString -> m ResponseToken
--   mayRespondJSON :: forall a. ToJSON a => Int -> [HTTP.Header] -> a -> m ResponseToken
--   mayThrow :: Int -> [HTTP.Header] -> LBS.ByteString -> m Error
--   respond :: ResponseToken -> m Response
--   throw :: Error -> m a

data Okapi m a where
  -- PARSERS
  Method :: HTTP.Method -> Okapi m ()
  Seg :: Text -> Okapi m ()
  SegParam :: forall a m. Read a => Okapi m a
  QueryParam :: forall a m. Read a => Text -> Okapi m a
  QueryFlag :: Text -> Okapi m Bool
  Header :: HTTP.HeaderName -> Okapi m Text
  BodyJSON :: forall a m. FromJSON a => Okapi m a
  BodyForm :: forall a m. FromForm a => Okapi m a
  -- RESPONDERS
  MayRespondPlainText :: Int -> [HTTP.Header] -> Okapi m (Text -> ResponseToken)
  MayRespondHTML :: Int -> [HTTP.Header] -> Okapi m (LBS.ByteString -> ResponseToken)
  MayRespondJSON :: forall a m. ToJSON a => Int -> [HTTP.Header] -> Okapi m (a -> ResponseToken)
  MayThrow :: Int -> [HTTP.Header] -> Okapi m (LBS.ByteString -> Error)
  Respond :: ResponseToken -> Okapi m Response
  Throw :: Error -> Okapi m a
  -- METHODS
  Pure :: a -> Okapi m a
  Ap :: Okapi m (a -> b) -> Okapi m a -> Okapi m b
  Fail :: Okapi m a
  Choice :: Okapi m a -> Okapi m a -> Okapi m a
  Bind :: Okapi m a -> (a -> Okapi m b) -> Okapi m b
  Action :: m a -> Okapi m a

instance Functor (Okapi m) where
  fmap = Ap . Pure

instance Applicative (Okapi m) where
  pure = Pure
  (<*>) = Ap

instance Alternative (Okapi m) where
  empty = Fail
  (<|>) = Choice

instance Monad (Okapi m) where
  (>>=) = Bind
  return = Pure

instance MonadPlus (Okapi m) where
  mzero = Fail
  mplus = Choice

-- type MonadOkapi m =
--   ( Functor m,
--     Applicative m,
--     Alternative m,
--     Monad m,
--     MonadPlus m
--   )

action :: m a -> Okapi m a
action = Action

method :: HTTP.Method -> Okapi m ()
method = Method

get :: Okapi m ()
get = Method "GET"

post :: Okapi m ()
post = Method "POST"

put :: Okapi m ()
put = Method "PUT"

delete :: Okapi m ()
delete = Method "DELETE"

seg :: Text -> Okapi m ()
seg = Seg

segParam :: forall a m. Read a => Okapi m a
segParam = SegParam

queryParam :: forall a m. Read a => Text -> Okapi m a
queryParam = QueryParam

queryFlag :: Text -> Okapi m Bool
queryFlag = QueryFlag

header :: HTTP.HeaderName -> Okapi m Text
header = Header

bodyJSON :: forall a m. FromJSON a => Okapi m a
bodyJSON = BodyJSON

bodyForm :: forall a m. FromForm a => Okapi m a
bodyForm = BodyForm

mayRespondJSON :: forall a m. ToJSON a => Int -> [HTTP.Header] -> Okapi m (a -> ResponseToken)
mayRespondJSON = MayRespondJSON

mayRespondHTML :: Int -> [HTTP.Header] -> Okapi m (LBS.ByteString -> ResponseToken)
mayRespondHTML = MayRespondHTML

mayRespondPlainText :: Int -> [HTTP.Header] -> Okapi m (Text -> ResponseToken)
mayRespondPlainText = MayRespondPlainText

mayThrow :: forall m. Int -> [HTTP.Header] -> Okapi m (LBS.ByteString -> Error)
mayThrow = MayThrow

fail :: Okapi m a
fail = Fail

respond :: ResponseToken -> Okapi m Response
respond = Respond

throw :: Error -> Okapi m a
throw = Throw

optimize :: Okapi m Response -> Okapi m Response
optimize = undefined
