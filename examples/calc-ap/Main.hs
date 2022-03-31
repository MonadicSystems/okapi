{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Main where

import Control.Applicative (Applicative (liftA2), (<|>))
import Control.Applicative.Combinators (choice)
import Data.Aeson (ToJSON)
import Data.Text
import GHC.Generics (Generic)
import Okapi

main = runOkapi id 3000 calcAp

calcAp = get *> seg "calc" *> choice [addOp, subOp, mulOp, divOp]

addOp = seg "add" *> respondJSONAp [] (uncurry (+) <$> getArgs)

subOp = (seg "sub" <|> seg "minus") *> respondJSONAp [] (uncurry (-) <$> getArgs)

mulOp = seg "mul" *> respondJSONAp [] (uncurry (*) <$> getArgs)

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

divOp = seg "div" *> (getArgs >>= (\(x, y) -> if y == 0 then abort403 [] "Forbidden" else respondJSON [] $ DivResult (x `div` y) (x `mod` y)))

getArgs = getArgsFromPath <|> getArgsFromQueryParams
  where
    getArgsFromPath = liftA2 (,) (segParamAs @Int) (segParamAs @Int)
    getArgsFromQueryParams = liftA2 (,) (queryParamAs @Int "x") (queryParamAs @Int "y")
