{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON)
import Data.Text
import GHC.Generics (Generic)
import Okapi


main :: IO ()
main = runOkapi id 3000 calc

-- type Okapi a = OkapiT IO a

calc :: MonadOkapi m => m Response
calc = do
  get
  seg "calc"
  addOp <|> subOp <|> mulOp <|> divOp

addOp :: MonadOkapi m => m Response
addOp = do
  seg "add"
  (x, y) <- getArgs
  respondJSON [] $ x + y

subOp :: MonadOkapi m => m Response
subOp = do
  seg "sub" <|> seg "minus"
  (x, y) <- getArgs
  respondJSON [] $ x - y

mulOp :: MonadOkapi m => m Response
mulOp = do
  seg "mul"
  (x, y) <- getArgs
  respondJSON [] $ x * y

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

divOp :: MonadOkapi m => m Response
divOp = do
  seg "div"
  (x, y) <- getArgs
  if y == 0
    then abort403 [] "Forbidden"
    else respondJSON [] $ DivResult {answer = x `div` y, remainder = x `mod` y}

getArgs :: MonadOkapi m => m (Int, Int)
getArgs = getArgsFromPath <|> getArgsFromQueryParams
  where
    getArgsFromPath :: MonadOkapi m => m (Int, Int)
    getArgsFromPath = do
      x <- segParamAs @Int
      y <- segParamAs @Int
      pure (x, y)

    getArgsFromQueryParams :: MonadOkapi m => m (Int, Int)
    getArgsFromQueryParams = do
      x <- queryParamAs @Int "x"
      y <- queryParamAs @Int "y"
      pure (x, y)
