{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonadComprehensions #-}
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

calc = [result | _ <- get, _ <- seg "calc", result <- addOp <|> subOp <|> mulOp <|> divOp]

addOp = [x + y | _ <- seg "add", (x, y) <- getArgs] >>= okJSON []

subOp = [x - y | _ <- seg "sub" <|> seg "minus", (x, y) <- getArgs] >>= okJSON []

mulOp = [x * y | _ <- seg "mul", (x, y) <- getArgs] >>= okJSON []

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

divOp =
  [(x, y) | _ <- seg "div", (x, y) <- getArgs]
    >>= ( \(x, y) ->
            if y == 0
              then error403 [] "Forbidden"
              else okJSON [] $ DivResult {answer = x `div` y, remainder = x `mod` y}
        )

getArgs =
  [(x, y) | x <- segParam, y <- segParam]
    <|> [(x, y) | x <- queryParam "x", y <- queryParam "y"]
