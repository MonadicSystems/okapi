{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Applicative (Applicative (liftA2), (<|>))
import Control.Applicative.Combinators (choice)
import Data.Aeson (ToJSON, encode)
import Data.Text
import GHC.Generics (Generic)
import Okapi

main :: IO ()
main = runOkapi id 3000 (genServer calcAp)

calcAp :: Parser Response
calcAp = getP *> segP "calc" *> choice [addOp, subOp, mulOp, divOp]

addOp :: Parser Response
addOp = segP "add" *> respondJSONP [] (uncurry (+) <$> getArgs)

subOp :: Parser Response
subOp = (segP "sub" <|> segP "minus") *> respondJSONP [] (uncurry (-) <$> getArgs)

mulOp :: Parser Response
mulOp = segP "mul" *> respondJSONP [] (uncurry (*) <$> getArgs)

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

divOp :: Parser Response
divOp =
    -- segP "div"
    -- *> (getArgs >>= (\(x, y) -> if y == 0 then abort403 [] "Forbidden" else respondJSON [] $ DivResult (x `div` y) (x `mod` y)))
    do
        segP "div"
        (x, y) <- getArgs
        -- ok <- respondP $ pure $ Response 200 [] ""
        -- err <- respondP $ pure $ Response 403 [] "Forbidden"
        respondP $ Response 403 [] "Forbidden"
        -- respondP $ if y == 0
        --     then Response 200 [] ""
        --     else Response 403 [] "Forbidden"

getArgs :: Parser (Int, Int)
getArgs = getArgsFromPath <|> getArgsFromQueryParams
  where
    getArgsFromPath :: Parser (Int, Int)
    getArgsFromPath = liftA2 (,) (segParamP @Int) (segParamP @Int)
    
    getArgsFromQueryParams :: Parser (Int, Int)
    getArgsFromQueryParams = liftA2 (,) (queryParamP @Int "x") (queryParamP @Int "y")
