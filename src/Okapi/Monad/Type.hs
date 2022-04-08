{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Okapi.Monad.Type where

import Control.Applicative
import Control.Monad
import Control.Monad.Except (MonadError (..))
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Morph
import Control.Monad.RWS (MonadReader (local), join)
import Control.Monad.Reader.Class (MonadReader (ask, reader))
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Log
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
import Network.HTTP.Types (status401)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (Settings)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WarpTLS (TLSSettings)
import qualified Network.Wai.Handler.WarpTLS as Warp
import Text.Read (readMaybe)
import Web.FormUrlEncoded (FromForm (fromForm), urlDecodeAsForm)
import qualified Web.HttpApiData as HTTP
import Formatting

{-
    Continue with next handler

        To go from Handler to handler, just sequence handlers in a do block like so:

        api = do
            post
            seg "api"
            login <|> register
            doSomeMore

    There will be two failure modes:

    Return Early

        This returns some error response immediately.

        It doesn't continue the sequence in the do block,
        and doesn't continue the sequence in the choice blocks either.

        It completely halts Okapi and forces to return a chosen response.
        For example, given the following sequence:

        api = do
            post
            seg "users"
            login <|> register
            doSomeMore

        If login returns a Early Return error register won't be tried and doSomeMore won't be tried either.
        Parsers containing this parser won't be tried either. For example:

        outerApi = api <|> someOtherApi

        If login in api fails with a return early error,
        register isn't tried and doSomeMore isn't tried,
        so basically the whole api parser fails.

        If the inner parser returns an early return error, than so does the parser wrapping it.
        So api would also return early and cause outerApi to return early without someOtherApi
        being tried.

        Basically, an early return error bubbles all the way to the top without stoping and returns a response.

    Skip

        This error type causes the current parser to fail,
        but if used in a group of choices the next one will be tried.

        If it bubbles up to a parser used in a choice block, other choices will be tried.

        Basically, this error type doesn't bubble up all the way to the top
        unless all possible branches fail.
-}

type Request =
  ( Bool, -- has the HTTP method been parsed yet?
    Bool, -- has body been parsed yet? Needed this because of possibilty of large bodies and streaming bodies
    Wai.Request
  )

-- type Response = Wai.Response

data Response = Response
  { responseStatus :: Int
  , responseHeaders :: [HTTP.Header]
  , responseBody :: LBS.ByteString
  }

data Error = Skip | Abort Response

-- instance Semigroup Error where
--   Skip <> someError = someError
--   someError <> Skip = someError
--   abort1@(Abort _) <> abort2@(Abort _) = abort1

-- instance Monoid Error where
--   mempty = Skip

newtype ServerT m a = ServerT {unOkapiT :: ExceptT Error (StateT Request m) a}
  deriving newtype
    ( MonadError Error,
      MonadState Request
    )

instance Functor m => Functor (ServerT m) where
  fmap :: (a -> b) -> ServerT m a -> ServerT m b
  fmap f okapiT =
    ServerT . ExceptT . StateT $
      ( fmap (\ ~(a, s') -> (f <$> a, s'))
          . runStateT (runExceptT $ unOkapiT okapiT)
      )
  {-# INLINE fmap #-}

instance Monad m => Applicative (ServerT m) where
  pure x = ServerT . ExceptT . StateT $ \s -> pure (Right x, s)
  {-# INLINEABLE pure #-}
  (ServerT (ExceptT (StateT mf))) <*> (ServerT (ExceptT (StateT mx))) = ServerT . ExceptT . StateT $ \s -> do
    ~(eitherF, s') <- mf s
    case eitherF of
      Left error -> pure (Left error, s)
      Right f -> do
        ~(eitherX, s'') <- mx s'
        case eitherX of
          Left error' -> pure (Left error', s')
          Right x -> pure (Right $ f x, s'')
  {-# INLINEABLE (<*>) #-}
  m *> k = m >>= \_ -> k
  {-# INLINE (*>) #-}

instance Monad m => Alternative (ServerT m) where
  empty = ServerT . ExceptT . StateT $ \s -> pure (Left Skip, s)
  {-# INLINE empty #-}
  (ServerT (ExceptT (StateT mx))) <|> (ServerT (ExceptT (StateT my))) = ServerT . ExceptT . StateT $ \s -> do
    (eitherX, stateX) <- mx s
    case eitherX of
      Left Skip -> do
        (eitherY, stateY) <- my s
        case eitherY of
          Left Skip -> pure (Left Skip, s)
          Left abort@(Abort _) -> pure (Left abort, s)
          Right y -> pure (Right y, stateY)
      Left abort@(Abort _) -> pure (Left abort, s)
      Right x -> pure (Right x, stateX)
  {-# INLINEABLE (<|>) #-}

instance Monad m => Monad (ServerT m) where
  return = pure
  {-# INLINEABLE return #-}
  (ServerT (ExceptT (StateT mx))) >>= f = ServerT . ExceptT . StateT $ \s -> do
    ~(eitherX, s') <- mx s
    case eitherX of
      Left error -> pure (Left error, s)
      Right x -> do
        ~(eitherResult, s'') <- runStateT (runExceptT $ unOkapiT $ f x) s'
        case eitherResult of
          Left error' -> pure (Left error', s')
          Right res -> pure (Right res, s'')
  {-# INLINEABLE (>>=) #-}

instance Monad m => MonadPlus (ServerT m) where
  mzero = ServerT . ExceptT . StateT $ \s -> pure (Left Skip, s)
  {-# INLINE mzero #-}
  (ServerT (ExceptT (StateT mx))) `mplus` (ServerT (ExceptT (StateT my))) = ServerT . ExceptT . StateT $ \s -> do
    (eitherX, stateX) <- mx s
    case eitherX of
      Left Skip -> do
        (eitherY, stateY) <- my s
        case eitherY of
          Left Skip -> pure (Left Skip, s)
          Left abort@(Abort _) -> pure (Left abort, s)
          Right y -> pure (Right y, stateY)
      Left abort@(Abort _) -> pure (Left abort, s)
      Right x -> pure (Right x, stateX)
  {-# INLINEABLE mplus #-}

instance MonadIO m => MonadIO (ServerT m) where
  liftIO = lift . liftIO

instance MonadReader r m => MonadReader r (ServerT m) where
  ask = lift ask
  local = mapOkapiT . local
    where
      mapOkapiT :: (m (Either Error a, Request) -> n (Either Error b, Request)) -> ServerT m a -> ServerT n b
      mapOkapiT f okapiT = ServerT . ExceptT . StateT $ f . runStateT (runExceptT $ unOkapiT okapiT)
  reader = lift . reader

instance MonadTrans ServerT where
  lift :: Monad m => m a -> ServerT m a
  lift action = ServerT . ExceptT . StateT $ \s -> do
    result <- action
    pure (Right result, s)

instance MFunctor ServerT where
  hoist :: Monad m => (forall a. m a -> n a) -> ServerT m b -> ServerT n b
  hoist nat okapiT = ServerT . ExceptT . StateT $ (nat . runStateT (runExceptT $ unOkapiT okapiT))

type MonadServer m =
  ( Functor m,
    Applicative m,
    Alternative m,
    Monad m,
    MonadPlus m,
    MonadIO m,
    MonadError Error m,
    MonadState Request m
  )

-- newtype PathT r r' m a = PathT { runPathT :: PureLoggingT (Format r (Text -> r')) m a }

-- instance Semigroup (Format r (Text -> r')) where

-- instance Monoid (Format r (r') where


-- type MonadPath r r' m =
--   ( Functor m
--   , Applicative m
--   , Alternative m
--   , Monad m
--   , MonadPlus m
--   , MonadLog (Format r (Text -> r')) m
--   )
