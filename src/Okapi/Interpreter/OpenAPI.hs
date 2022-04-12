{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Okapi.Interpreter.OpenAPI where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity (Identity)
import Control.Monad.State (State)
import Control.Monad.State.Class
import Control.Monad.Trans.State (StateT)
import Data.OpenApi (OpenApi)
import qualified Okapi.Grammar as Grammar
import Okapi.Type

data OpenAPIState m a = OpenAPIState
  { openAPIStateOpenApi :: OpenApi,
    openAPIStateGrammar :: Grammar.Okapi m a
  }

-- type MonadOpenAPI m =
--   ( Functor m,
--     Applicative m,
--     Alternative m,
--     Monad m,
--     MonadPlus m,
--     MonadState OpenAPIState m
--   )

-- genOpenAPI :: State (OpenAPIState m a) OpenApi
-- genOpenAPI (Grammar.Method m) = method m
-- genOpenAPI (Grammar.Seg s) = seg s
-- genOpenAPI Grammar.SegParam = segParam
-- genOpenAPI (Grammar.QueryParam name) = queryParam name
-- genOpenAPI (Grammar.QueryFlag name) = queryParamFlag name
-- genOpenAPI (Grammar.Header headerName) = header headerName
-- genOpenAPI Grammar.BodyJSON = bodyJSON
-- genOpenAPI Grammar.BodyForm = bodyForm
-- genOpenAPI (Grammar.MayRespondPlainText status headers) = mayRespondPlainText status headers
-- genOpenAPI (Grammar.MayRespondHTML status headers) = mayRespondHTML status headers
-- genOpenAPI (Grammar.MayRespondJSON status headers) = mayRespondJSON status headers
-- genOpenAPI (Grammar.MayThrow status headers) = mayThrow status headers
-- genOpenAPI (Grammar.Throw error) = throwError error
-- genOpenAPI (Grammar.Respond token) = respond token
-- genOpenAPI Grammar.Fail = throwError Skip
-- genOpenAPI (Grammar.Pure v) = pure v
-- genOpenAPI (Grammar.Ap f x) = genOpenAPI f <*> genOpenAPI x
-- genOpenAPI (Grammar.Choice f g) = genOpenAPI f <|> genOpenAPI g
-- genOpenAPI (Grammar.Bind x f) = genOpenAPI (x >>= f)
-- genOpenAPI (Grammar.Action action) = action
