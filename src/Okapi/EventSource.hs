{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Okapi.EventSource
  ( ToSSE (..)
  , Event (..)
  , EventSource
  , newEventSource
  , sendEvent
  , sendValue
  , eventSourceAppUnagiChan
  )
where

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Control.Monad.IO.Class as IO
-- import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Function as Function
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.EventSource as EventSource

class ToSSE a where
  toSSE :: a -> Event

data Event
  = Event
      { eventName :: Maybe Text.Text,
        eventID :: Maybe Text.Text,
        eventData :: ByteString.ByteString
      }
  | CommentEvent ByteString.ByteString
  | CloseEvent
  deriving (Show, Eq)

type Chan a = (Unagi.InChan a, Unagi.OutChan a)

type EventSource = Chan Event

newEventSource :: IO EventSource
newEventSource = Unagi.newChan

sendValue :: ToSSE a => EventSource -> a -> IO ()
sendValue (inChan, _outChan) = Unagi.writeChan inChan . toSSE

sendEvent :: EventSource -> Event -> IO ()
sendEvent (inChan, _outChan) = Unagi.writeChan inChan

eventSourceAppUnagiChan :: Chan Event -> Wai.Application
eventSourceAppUnagiChan (inChan, _outChan) req sendResponse = do
  outChan <- IO.liftIO $ Unagi.dupChan inChan
  eventSourceAppIO (eventToServerEvent <$> Unagi.readChan outChan) req sendResponse

eventSourceAppIO :: IO EventSource.ServerEvent -> Wai.Application
eventSourceAppIO src _ sendResponse =
  sendResponse $
    Wai.responseStream
      HTTP.status200
      [(HTTP.hContentType, "text/event-stream")]
      $ \sendChunk flush -> do
        flush
        Function.fix $ \loop -> do
          se <- src
          case eventToBuilder se of
            Nothing -> return ()
            Just b -> sendChunk b >> flush >> loop

eventToBuilder :: EventSource.ServerEvent -> Maybe Builder.Builder
eventToBuilder (EventSource.CommentEvent txt) = Just $ field commentField txt
eventToBuilder (EventSource.RetryEvent n) = Just $ field retryField (Builder.string8 . show $ n)
eventToBuilder EventSource.CloseEvent = Nothing
eventToBuilder (EventSource.ServerEvent n i d) =
  Just $
    mappend (name n (evid i $ evdata (mconcat d) nl)) nl
  where
    name Nothing = id
    name (Just n') = mappend (field nameField n')
    evid Nothing = id
    evid (Just i') = mappend (field idField i')
    evdata d' = mappend (field dataField d')

nl :: Builder.Builder
nl = Builder.char7 '\n'

nameField, idField, dataField, retryField, commentField :: Builder.Builder
nameField = Builder.string7 "event:"
idField = Builder.string7 "id:"
dataField = Builder.string7 "data:"
retryField = Builder.string7 "retry:"
commentField = Builder.char7 ':'

-- | Wraps the text as a labeled field of an event stream.
field :: Builder.Builder -> Builder.Builder -> Builder.Builder
field l b = l `mappend` b `mappend` nl

eventToServerEvent :: Event -> EventSource.ServerEvent
eventToServerEvent Event {..} =
  EventSource.ServerEvent
    (Builder.byteString . Text.encodeUtf8 <$> eventName)
    (Builder.byteString . Text.encodeUtf8 <$> eventID)
    (Builder.word8 <$> ByteString.unpack eventData)
eventToServerEvent (CommentEvent comment) = EventSource.CommentEvent $ Builder.lazyByteString comment
eventToServerEvent CloseEvent = EventSource.CloseEvent
