{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module OkapiAPI (okapiApplication) where

import Control.Applicative ((<|>))
import Control.Monad.Combinators (optional)
import Lucid (ToHtml (..), renderBS)
import Network.HTTP.Types (Header)
import Network.Wai (Application)
import Okapi
import Template

okapiApplication :: Application
okapiApplication = makeOkapiApp id okapiAPI

type Okapi a = OkapiT IO a

okapiAPI :: Okapi Response
okapiAPI = do
  get
  seg "okapi"
  _ <- optional $ seg "" -- Trailing slash...This is needed for now but we can change it so it's automatically handled
  okLucid [] (Greeting "okapi") <|> calc

calc :: Okapi Response
calc = do
  seg "calc"
  addOp <|> subOp <|> mulOp <|> divOp

addOp :: Okapi Response
addOp = do
  seg "add"
  (x, y) <- getArgs
  okLucid [] $ AddResult x y $ x + y

subOp :: Okapi Response
subOp = do
  seg "sub" <|> seg "minus"
  (x, y) <- getArgs
  okLucid [] $ SubResult x y $ x - y

mulOp :: Okapi Response
mulOp = do
  seg "mul"
  (x, y) <- getArgs
  okLucid [] $ MulResult x y $ x * y

divOp :: Okapi Response
divOp = do
  seg "div"
  (x, y) <- getArgs
  if y == 0
    then okLucid [] DivByZero
    else
      okLucid [] $
        DivResult
          { dividend = x,
            divisor = y,
            answer = x `div` y,
            remainder = x `mod` y
          }

getArgs :: Okapi (Int, Int)
getArgs = getArgsFromPath <|> getArgsFromQueryParams
  where
    getArgsFromPath :: Okapi (Int, Int)
    getArgsFromPath = do
      x <- segParam @Int
      y <- segParam @Int
      pure (x, y)

    getArgsFromQueryParams :: Okapi (Int, Int)
    getArgsFromQueryParams = do
      x <- queryParam @Int "x"
      y <- queryParam @Int "y"
      pure (x, y)
