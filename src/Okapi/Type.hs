module Okapi.Type where

import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Types as HTTP

data Response = Response
  { responseStatus :: Int,
    responseHeaders :: [HTTP.Header],
    responseBody :: LBS.ByteString
  }
  deriving (Eq, Show)

data ResponseToken = ResponseToken Int [HTTP.Header] LBS.ByteString
  deriving (Eq, Show)

data Error = Skip | Abort Response
  deriving (Eq, Show)
