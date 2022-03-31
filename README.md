# Okapi

A monadic parser for HTTP requests.

## Introduction

**Okapi** is a microframework for building web servers in [Haskell](https://haskell.org) based on *monadic parsing*.
In contrast to other web frameworks in the Haskell ecosystem, Okapi is primarily concerned with being easy to understand and use, instead of extreme type safety.
Here's an example of a simple web server:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Okapi

main :: IO ()
main = runOkapi id 3000 greet

greet = do
  seg "greet"
  name <- segParam
  respondPlainText [] $ "Hello " <> name <> "! I'm Okapi."
```

Running this code will start a server on [localhost:3000](http://localhost:3000.org).
If you go to [http://localhost:3000/greeting/Bob]() the server will respond with `Hello Bob! I'm Okapi.` in plain text format.

Okapi is a [monadic parser](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) for HTTP requests. This means it can be used with all `Applicative`, `Alternative`, and `Monad` typeclass methods, plus other Haskell idioms like [parser combinators](https://hackage.haskell.org/package/parser-combinators).

Here's a more complicated example that implements a calculator API. Type annotations are added to make the code easier to follow:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON)
import Data.Text
import GHC.Generics (Generic)
import Okapi


main :: IO ()
main = runOkapi id 3000 calc

type Okapi a = OkapiT IO a

calc :: Okapi Response
calc = do
  get
  seg "calc"
  addOp <|> subOp <|> mulOp <|> divOp

addOp :: Okapi Response
addOp = do
  seg "add"
  (x, y) <- getArgs
  respondJSON [] $ x + y

subOp :: Okapi Response
subOp = do
  seg "sub" <|> seg "minus"
  (x, y) <- getArgs
  respondJSON [] $ x - y

mulOp :: Okapi Response
mulOp = do
  seg "mul"
  (x, y) <- getArgs
  respondJSON [] $ x * y

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

divOp :: Okapi Response
divOp = do
  seg "div"
  (x, y) <- getArgs
  if y == 0
    then abort403 [] "Forbidden"
    else respondJSON [] $ DivResult {answer = x `div` y, remainder = x `mod` y}

getArgs :: Okapi (Int, Int)
getArgs = getArgsFromPath <|> getArgsFromQueryParams
  where
    getArgsFromPath :: Okapi (Int, Int)
    getArgsFromPath = do
      x <- segParamAs @Int
      y <- segParamAs @Int
      pure (x, y)

    getArgsFromQueryParams :: Okapi (Int, Int)
    getArgsFromQueryParams = do
      x <- queryParamAs @Int "x"
      y <- queryParamAs @Int "y"
      pure (x, y)
```

Okapi is very flexible. You could also define the above without `do` notation or type annotations:

```haskell
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

calcNoDo = get >> seg "calc" >> choice [addOp, subOp, mulOp, divOp]

addOp = seg "add" >> (getArgs >>= (\(x, y) -> respondJSON [] $ x + y))

subOp = (seg "sub" <|> seg "minus") >> (getArgs >>= (\(x, y) -> respondJSON [] $ x - y))

mulOp = seg "mul" >> (getArgs >>= (\(x, y) -> respondJSON [] $ x * y))

data DivResult = DivResult
  { answer :: Int,
    remainder :: Int
  }
  deriving (Eq, Show, Generic, ToJSON)

divOp = seg "div" >> (getArgs >>= (\(x, y) -> if y == 0 then abort403 [] "Forbidden" else respondJSON [] $ DivResult (x `div` y) (x `mod` y)))

getArgs = getArgsFromPath <|> getArgsFromQueryParams
  where
    getArgsFromPath = segParamAs @Int >>= (\x -> segParamAs @Int >>= (\y -> pure (x, y)))
    getArgsFromQueryParams = queryParamAs @Int "x" >>= (\x -> queryParamAs @Int "y" >>= (\y -> pure (x, y)))
```

Don't like monads at all? You can use Okapi as an [applicative parser](https://eli.thegreenplace.net/2017/deciphering-haskells-applicative-and-monadic-parsers/) too:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative (liftA2, (<|>))
import Control.Applicative.Combinators (choice)
import Data.Aeson (ToJSON)
import Data.Text
import GHC.Generics (Generic)
import Okapi

main :: IO ()
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

```

> Notice how `respondJSON` is replaced with it's applicative variant `respondJSONAp` in the applicative parsers.
> Okapi exposes other applicative variants of the monadic parsers where necessary.
> They will have the same name as their monadic cousins, but ending with `Ap` for `Applicative`.

> The parser for the div calculator is still monadic because it needs to "inspect" the value inside the context and make an effectful decision based on the value. Some people call this a *data-dependent effect*. For parsers that don't have data-dependent effects, they can be applicative if you like.


As you can see, Okapi's parsing functions are very modular and can be easily composed with one another to create parsing primitives tailored specifically to your needs.
With Okapi, and the rest of the amazing Haskell ecosystem, you can create anything from simple website servers to complex APIs for web apps.
All you need to get started is basic knowledge about the structure of HTTP requests and an idea of how monadic parsing works.
