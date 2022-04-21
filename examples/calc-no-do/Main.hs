{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad.Combinators (choice)
import Data.Aeson (ToJSON)
import Data.Text
import GHC.Generics (Generic)
import Okapi

main :: IO ()
main = runOkapi id 3000 calcNoDo

calcNoDo = get >> seg "calc" >> choice [addOp, subOp, mulOp, divOp]

addOp = seg "add" >> (getArgs >>= (\(x, y) -> okJSON [] $ x + y))

subOp = (seg "sub" <|> seg "minus") >> (getArgs >>= (\(x, y) -> okJSON [] $ x - y))

mulOp = seg "mul" >> (getArgs >>= (\(x, y) -> okJSON [] $ x * y))

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

divOp =
  seg "div"
    >> ( getArgs
           >>= ( \(x, y) ->
                   if y == 0
                     then error403 [] "Forbidden"
                     else okJSON [] $ DivResult (x `div` y) (x `mod` y)
               )
       )

getArgs = getArgsFromPath <|> getArgsFromQueryParams
  where
    getArgsFromPath = segParam >>= (\x -> segParam >>= (\y -> pure (x, y)))
    getArgsFromQueryParams = queryParam "x" >>= (\x -> queryParam "y" >>= (\y -> pure (x, y)))
