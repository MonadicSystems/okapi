{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON)
import Data.Text
import GHC.Generics (Generic)
import Okapi

main :: IO ()
main = runOkapi id 3000 calc

type Okapi a = OkapiT IO a

calc :: Okapi Result
calc = do
  get
  seg "calc"
  addOp <|> subOp <|> mulOp <|> divOp

addOp :: Okapi Result
addOp = do
  seg "add"
  (x, y) <- getArgs
  okJSON [] $ x + y

subOp :: Okapi Result
subOp = do
  seg "sub" <|> seg "minus"
  (x, y) <- getArgs
  okJSON [] $ x - y

mulOp :: Okapi Result
mulOp = do
  seg "mul"
  (x, y) <- getArgs
  okJSON [] $ x * y

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

divOp :: Okapi Result
divOp = do
  seg "div"
  (x, y) <- getArgs
  if y == 0
    then error403 [] "Forbidden"
    else okJSON [] $ DivResult {answer = x `div` y, remainder = x `mod` y}

getArgs :: Okapi (Int, Int)
getArgs = getArgsFromPath <|> getArgsFromQueryParams
  where
    getArgsFromPath :: Okapi (Int, Int)
    getArgsFromPath = do
      x <- segParam
      y <- segParam
      pure (x, y)

    getArgsFromQueryParams :: Okapi (Int, Int)
    getArgsFromQueryParams = do
      x <- queryParam "x"
      y <- queryParam "y"
      pure (x, y)
