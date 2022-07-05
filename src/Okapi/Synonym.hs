module Okapi.Synonym where

import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP

type Path = [Text.Text]

type Headers = [HTTP.Header]

type QueryItem = (Text.Text, Maybe Text.Text)

type Query = [QueryItem]

type Cookie = (Text.Text, Text.Text)

type Cookies = [Cookie]
