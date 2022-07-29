{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON)
import Data.Function ((&))
import Data.Text
import GHC.Generics (Generic)
import Okapi

main :: IO ()
main = runOkapi id notFound 3000 calc

type Okapi a = OkapiT IO a

calc :: Okapi Response
calc = do
  get
  pathSeg "calc"
  addOp <|> subOp <|> mulOp <|> divOp

addOp :: Okapi Response
addOp = do
  pathSeg "add"
  (x, y) <- getArgs
  respond $
    aeson (x + y) ok

subOp :: Okapi Response
subOp = do
  pathSeg "sub" <|> pathSeg "minus"
  (x, y) <- getArgs
  respond $
    ok & aeson (x - y)

mulOp :: Okapi Response
mulOp = do
  pathSeg "mul"
  (x, y) <- getArgs
  ok & aeson (x * y) & respond

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

divOp :: Okapi Response
divOp = do
  pathSeg "div"
  (x, y) <- getArgs
  if y == 0
    then throw forbidden
    else
      ok
        & aeson DivResult {answer = x `div` y, remainder = x `mod` y}
        & respond

getArgs :: Okapi (Int, Int)
getArgs = getArgsFromPath <|> getArgsFromQueryParams
  where
    getArgsFromPath :: Okapi (Int, Int)
    getArgsFromPath = do
      x <- pathParam
      y <- pathParam
      pure (x, y)

    getArgsFromQueryParams :: Okapi (Int, Int)
    getArgsFromQueryParams = do
      x <- queryParam "x"
      y <- queryParam "y"
      pure (x, y)
