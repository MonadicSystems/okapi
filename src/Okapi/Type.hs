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

module Okapi.Type where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.State as StateT
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Vault.Lazy as Vault
import qualified GHC.Natural as Natural
import qualified Network.HTTP.Types as HTTP

type Path = [Text.Text]

type Headers = [HTTP.Header]

type QueryItem = (Text.Text, Maybe Text.Text)

type Query = [QueryItem]

data Request = Request
  { requestMethod :: HTTP.Method,
    requestPath :: Path,
    requestQuery :: Query,
    requestBody :: IO LazyByteString.ByteString,
    requestHeaders :: Headers,
    requestVault :: Vault.Vault,
    requestMethodParsed :: Bool,
    requestBodyParsed :: Bool
  }

data Response = Response
  { responseStatus :: Natural.Natural,
    responseHeaders :: Headers,
    responseBody :: LazyByteString.ByteString
  }

data Error = Skip | Abort Response

newtype OkapiT m a = OkapiT {unOkapiT :: ExceptT.ExceptT Error (StateT.StateT Request m) a}
  deriving newtype
    ( Except.MonadError Error,
      State.MonadState Request
    )

instance Functor m => Functor (OkapiT m) where
  fmap :: (a -> b) -> OkapiT m a -> OkapiT m b
  fmap f okapiT =
    OkapiT . ExceptT.ExceptT . StateT.StateT $
      ( fmap (\ ~(a, s') -> (f <$> a, s'))
          . StateT.runStateT (ExceptT.runExceptT $ unOkapiT okapiT)
      )
  {-# INLINE fmap #-}

instance Monad m => Applicative (OkapiT m) where
  pure x = OkapiT . ExceptT.ExceptT . StateT.StateT $ \s -> pure (Right x, s)
  {-# INLINEABLE pure #-}
  (OkapiT (ExceptT.ExceptT (StateT.StateT mf))) <*> (OkapiT (ExceptT.ExceptT (StateT.StateT mx))) = OkapiT . ExceptT.ExceptT . StateT.StateT $ \s -> do
    ~(eitherF, s') <- mf s
    case eitherF of
      Left error -> pure (Left error, s)
      Right f -> do
        ~(eitherX, s'') <- mx s'
        case eitherX of
          Left error' -> pure (Left error', s')
          Right x -> pure (Right $ f x, s'')
  {-# INLINEABLE (<*>) #-}
  m *> k = m >> k
  {-# INLINE (*>) #-}

instance Monad m => Applicative.Alternative (OkapiT m) where
  empty = OkapiT . ExceptT.ExceptT . StateT.StateT $ \s -> pure (Left Skip, s)
  {-# INLINE empty #-}
  (OkapiT (ExceptT.ExceptT (StateT.StateT mx))) <|> (OkapiT (ExceptT.ExceptT (StateT.StateT my))) = OkapiT . ExceptT.ExceptT . StateT.StateT $ \s -> do
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

instance Monad m => Monad (OkapiT m) where
  return = pure
  {-# INLINEABLE return #-}
  (OkapiT (ExceptT.ExceptT (StateT.StateT mx))) >>= f = OkapiT . ExceptT.ExceptT . StateT.StateT $ \s -> do
    ~(eitherX, s') <- mx s
    case eitherX of
      Left error -> pure (Left error, s)
      Right x -> do
        ~(eitherResult, s'') <- StateT.runStateT (ExceptT.runExceptT $ unOkapiT $ f x) s'
        case eitherResult of
          Left error' -> pure (Left error', s')
          Right res -> pure (Right res, s'')
  {-# INLINEABLE (>>=) #-}

instance Monad m => Monad.MonadPlus (OkapiT m) where
  mzero = OkapiT . ExceptT.ExceptT . StateT.StateT $ \s -> pure (Left Skip, s)
  {-# INLINE mzero #-}
  (OkapiT (ExceptT.ExceptT (StateT.StateT mx))) `mplus` (OkapiT (ExceptT.ExceptT (StateT.StateT my))) = OkapiT . ExceptT.ExceptT . StateT.StateT $ \s -> do
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

instance IO.MonadIO m => IO.MonadIO (OkapiT m) where
  liftIO = Morph.lift . IO.liftIO

instance Reader.MonadReader r m => Reader.MonadReader r (OkapiT m) where
  ask = Morph.lift Reader.ask
  local = mapOkapiT . Reader.local
    where
      mapOkapiT :: (m (Either Error a, Request) -> n (Either Error b, Request)) -> OkapiT m a -> OkapiT n b
      mapOkapiT f okapiT = OkapiT . ExceptT.ExceptT . StateT.StateT $ f . StateT.runStateT (ExceptT.runExceptT $ unOkapiT okapiT)
  reader = Morph.lift . Reader.reader

instance Morph.MonadTrans OkapiT where
  lift :: Monad m => m a -> OkapiT m a
  lift action = OkapiT . ExceptT.ExceptT . StateT.StateT $ \s -> do
    result <- action
    pure (Right result, s)

instance Morph.MFunctor OkapiT where
  hoist :: Monad m => (forall a. m a -> n a) -> OkapiT m b -> OkapiT n b
  hoist nat okapiT = OkapiT . ExceptT.ExceptT . StateT.StateT $ (nat . StateT.runStateT (ExceptT.runExceptT $ unOkapiT okapiT))

type MonadOkapi m =
  ( Functor m,
    Applicative m,
    Applicative.Alternative m,
    Monad m,
    Monad.MonadPlus m,
    IO.MonadIO m,
    Except.MonadError Error m,
    State.MonadState Request m
  )
