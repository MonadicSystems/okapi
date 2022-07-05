{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Okapi.Response where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified GHC.Natural as Natural
import qualified Lucid
import qualified Network.HTTP.Types as HTTP
import qualified Okapi.Event as Event
import Okapi.Synonym

data Response = Response
  { responseStatus :: Natural.Natural,
    responseHeaders :: Headers,
    responseBody :: ResponseBody
  }

data ResponseBody
  = ResponseBodyRaw LazyByteString.ByteString
  | ResponseBodyFile FilePath
  | ResponseBodyEventSource Event.EventSource

setResponseStatus :: Natural.Natural -> Response -> Response
setResponseStatus status response = response {responseStatus = status}

setResponseHeaders :: Headers -> Response -> Response
setResponseHeaders headers response = response {responseHeaders = headers}

setResponseHeader :: HTTP.Header -> Response -> Response
setResponseHeader header response@Response {..} =
  response {responseHeaders = update header responseHeaders}
  where
    update :: forall a b. Eq a => (a, b) -> [(a, b)] -> [(a, b)]
    update pair [] = [pair]
    update pair@(key, value) (pair'@(key', value') : ps) =
      if key == key'
        then pair : ps
        else pair' : update pair ps

setResponseBody :: ResponseBody -> Response -> Response
setResponseBody body response = response {responseBody = body}

-- TODO: setResponseCookie
ok :: Response
ok =
  let responseStatus = 200
      responseHeaders = []
      responseBody = ResponseBodyRaw ""
   in Response {..}

okHTML :: LazyByteString.ByteString -> Response
okHTML html =
  ok
    & setResponseHeader ("Content-Type", "text/html")
    & setResponseBody (ResponseBodyRaw html)

okPlainText :: Text.Text -> Response
okPlainText text =
  let raw = LazyByteString.fromStrict . Text.encodeUtf8 $ text
   in ok
        & setResponseHeader ("Content-Type", "text/plain")
        & setResponseBody (ResponseBodyRaw raw)

okJSON :: forall a. Aeson.ToJSON a => a -> Response
okJSON value =
  let raw = Aeson.encode value
   in ok
        & setResponseHeader ("Content-Type", "application/json")
        & setResponseBody (ResponseBodyRaw raw)

okLucid :: forall a. Lucid.ToHtml a => a -> Response
okLucid value =
  let raw = Lucid.renderBS . Lucid.toHtml $ value
   in okHTML raw

okFile :: FilePath -> Response
okFile filePath = ok & setResponseBody (ResponseBodyFile filePath)

okEventSource :: Event.EventSource -> Response
okEventSource eventSource = ok & setResponseBody (ResponseBodyEventSource eventSource)

noContent :: Response
noContent =
  let responseStatus = 204
      responseHeaders = []
      responseBody = ResponseBodyRaw ""
   in Response {..}

unauthorized :: Response
unauthorized =
  let responseStatus = 401
      responseHeaders = []
      responseBody = ResponseBodyRaw ""
   in Response {..}

internalServerError :: Response
internalServerError =
  let responseStatus = 500
      responseHeaders = []
      responseBody = ResponseBodyRaw ""
   in Response {..}

-- TODO: Change type of URL?
redirectTo :: Char8.ByteString -> Response
redirectTo url =
  let responseStatus = 302
      responseHeaders = [("Location", url)]
      responseBody = ResponseBodyRaw ""
   in Response {..}
