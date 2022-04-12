{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON)
import Data.Text
import GHC.Generics (Generic)
import Okapi.Grammar
import Okapi.Interpreter.Server (genServer, runOkapi)
import Okapi.Type
import Prelude hiding (error)

main :: IO ()
main = runOkapi id 3000 (genServer calc)

calc :: Okapi m Response
calc = do
  get
  seg "calc"
  addOp <|> subOp <|> mulOp <|> divOp

addOp :: Okapi m Response
addOp = do
  seg "add"
  (x, y) <- getArgs
  response <- mayRespondJSON 200 []
  respond $ response $ x + y

subOp :: Okapi m Response
subOp = do
  seg "sub" <|> seg "minus"
  (x, y) <- getArgs
  response <- mayRespondJSON 200 []
  respond $ response $ x - y

mulOp :: Okapi m Response
mulOp = do
  seg "mul"
  (x, y) <- getArgs
  response <- mayRespondJSON 200 [] 
  respond $ response $ x * y

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

divOp :: Okapi m Response
divOp = do
  seg "div"
  (x, y) <- getArgs
  err <- mayThrow 403 []
  ok <- mayRespondJSON 200 []
  if y == 0
    then throw $ err "Forbidden"
    else respond $ ok $ DivResult {answer = x `div` y, remainder = x `mod` y}

getArgs :: Okapi m (Int, Int)
getArgs = getArgsFromPath <|> getArgsFromQueryParams
  where
    getArgsFromPath :: Okapi m (Int, Int)
    getArgsFromPath = do
      x <- segParam @Int
      y <- segParam @Int
      pure (x, y)

    getArgsFromQueryParams :: Okapi m (Int, Int)
    getArgsFromQueryParams = do
      x <- queryParam @Int "x"
      y <- queryParam @Int "y"
      pure (x, y)
