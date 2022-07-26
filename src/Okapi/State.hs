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
    stateRequestMethodParsed :: Bool,
    stateRequestBodyParsed   :: Bool,
    stateResponded           :: Bool
  }

data Request = Request
  { requestMethod  :: HTTP.Method,
    requestPath    :: Path,
    requestQuery   :: Query,
    requestBody    :: IO LazyByteString.ByteString,
    requestHeaders :: Headers,
    requestVault   :: Vault.Vault
  }
