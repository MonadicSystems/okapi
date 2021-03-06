{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Okapi.State where

import qualified Control.Applicative as Applicative
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Vault.Lazy as Vault
import qualified GHC.Natural as Natural
import qualified Network.HTTP.Types as HTTP
import qualified Okapi.Event as Event
import Okapi.Synonym

-- TODO: Just use Raw Wai Request
data State = State
  { stateRequest             :: Request,
    -- TODO: Remove state checkers??
    stateRequestMethodParsed :: Bool,
    stateRequestBodyParsed   :: Bool,
    stateResponded           :: Bool
    -- add HTTPDataStore???
  }

{-
TODO: HTTPDataStore? Not really needed because you can just pass data normally or store in own monad.
One benefit is that the data is available to all sub-branches without explicitly passing them to every sub-branch.

-- This data structure should be hidden from user
data HTTPDataStore = HTTPDataStore
  { pathStore   :: (Map Text Text)
  , queryStore  :: (Map Text Text)
  , headerStore :: (Map Text Text)
  }

-- Can only store parsed information
storePathParam :: forall a. FromHttpApiData a => Text -> Okapi a
storePathParam = ...

storeQueryParam :: ... => Text -> Okapi a

storeHeader :: ... => Text -> Okapi a

-- Can fail on Map lookup and data conversion
findPathParam :: forall a. FromHttpApiData a => Okapi a
-}

data Request = Request
  { requestMethod  :: HTTP.Method,
    requestPath    :: Path,
    requestQuery   :: Query,
    requestBody    :: IO LazyByteString.ByteString,
    requestHeaders :: Headers,
    requestVault   :: Vault.Vault
  }
