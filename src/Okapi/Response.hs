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

-- RESPONSE SETTERS (Hidden)

setResponseStatus :: Natural.Natural -> Response -> Response
setResponseStatus status response = response {responseStatus = status}

setResponseHeaders :: Headers -> Response -> Response
setResponseHeaders headers response = response {responseHeaders = headers}

-- TODO: setResponseCookie

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

-- BASE RESPONSES

ok :: Response
ok =
  let responseStatus = 200
      responseHeaders = []
      responseBody = ResponseBodyRaw "OK"
   in Response {..}

noContent :: Response
noContent =
  let responseStatus = 204
      responseHeaders = []
      responseBody = ResponseBodyRaw "No Content"
   in Response {..}

notFound :: Response
notFound =
  let responseStatus = 404
      responseHeaders = []
      responseBody = ResponseBodyRaw "Not Found"
    in Response {..}

unauthorized :: Response
unauthorized =
  let responseStatus = 401
      responseHeaders = []
      responseBody = ResponseBodyRaw "Unauthorized"
   in Response {..}

internalServerError :: Response
internalServerError =
  let responseStatus = 500
      responseHeaders = []
      responseBody = ResponseBodyRaw "Internal Server Error"
   in Response {..}

-- TODO: Change type of URL?
redirectTo :: Char8.ByteString -> Response
redirectTo url =
  let responseStatus = 302
      responseHeaders = [("Location", url)]
      responseBody = ResponseBodyRaw ""
   in Response {..}

-- RESPONSE BODY MODIFIERS

plaintext :: Text.Text -> Response -> Response
plaintext text response =
  response
    & setResponseHeader ("Content-Type", "text/plain")
    & setResponseBody (ResponseBodyRaw $ LazyByteString.fromStrict . Text.encodeUtf8 $ text)

html :: LazyByteString.ByteString -> Response -> Response
html htmlRaw response =
  response
    & setResponseBody (ResponseBodyRaw htmlRaw)
    & setResponseHeader ("Content-Type", "text/html")

json :: LazyByteString.ByteString -> Response -> Response
json bytes response =
  response
    & setResponseHeader ("Content-Type", "application/json")
    & setResponseBody (ResponseBodyRaw bytes)

file :: FilePath -> Response -> Response
file path = setResponseBody (ResponseBodyFile path) -- TODO: setHeader???

eventSource :: Event.EventSource -> Response -> Response
eventSource source response =
  response &
    setResponseBody (ResponseBodyEventSource source)

aeson :: forall a. Aeson.ToJSON a => a -> Response -> Response
aeson = json . Aeson.encode

lucid :: forall a. Lucid.ToHtml a => a -> Response -> Response
lucid = html . Lucid.renderBS . Lucid.toHtml
