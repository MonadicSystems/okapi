{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad.Combinators (choice)
import Data.Aeson (ToJSON)
import Data.Text
import GHC.Generics (Generic)
import Okapi

main :: IO ()
main = runOkapi id 3000 calcNoDo

type Okapi a = OkapiT IO a

calcNoDo :: Okapi Response
calcNoDo = get >> seg "calc" >> choice [addOp, subOp, mulOp, divOp]

addOp :: Okapi Response
addOp = seg "add" >> (getArgs >>= (\(x, y) -> respondJSON [] $ x + y))

subOp :: Okapi Response
subOp = (seg "sub" <|> seg "minus") >> (getArgs >>= (\(x, y) -> respondJSON [] $ x - y))

mulOp :: Okapi Response
mulOp = seg "mul" >> (getArgs >>= (\(x, y) -> respondJSON [] $ x * y))

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

divOp :: Okapi Response
divOp = seg "div" >> (getArgs >>= (\(x, y) -> if y == 0 then abort403 [] "Forbidden" else respondJSON [] $ DivResult (x `div` y) (x `mod` y)))

getArgs :: Okapi (Int, Int)
getArgs = getArgsFromPath <|> getArgsFromQueryParams
  where
    getArgsFromPath :: Okapi (Int, Int)
    getArgsFromPath = segParamAs @Int >>= (\x -> segParamAs @Int >>= (\y -> pure (x, y)))

    getArgsFromQueryParams :: Okapi (Int, Int)
    getArgsFromQueryParams = queryParamAs @Int "x" >>= (\x -> queryParamAs @Int "y" >>= (\y -> pure (x, y)))