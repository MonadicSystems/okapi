{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Okapi.EventSource where

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.Wai as Wai
import qualified Network.Wai.EventSource as EventSource

class ToSSE a where
  toSSE :: a -> Event

data Event
  = Event
      { eventName :: Maybe Text.Text,
        eventID :: Maybe Text.Text,
        eventData :: LazyByteString.ByteString
      }
  | CommentEvent LazyByteString.ByteString
  | CloseEvent

type Chan a = (Unagi.InChan a, Unagi.OutChan a)

type EventSource = Chan Event

-- type EventSourcePool = Map.Map Int EventSource

eventToServerEvent :: Event -> EventSource.ServerEvent
eventToServerEvent Event {..} =
  EventSource.ServerEvent
    (Builder.byteString . Text.encodeUtf8 <$> eventName)
    (Builder.byteString . Text.encodeUtf8 <$> eventID)
    (Builder.word8 <$> LazyByteString.unpack eventData)
eventToServerEvent (CommentEvent comment) = EventSource.CommentEvent $ Builder.lazyByteString comment
eventToServerEvent CloseEvent = EventSource.CloseEvent

eventSourceAppUnagiChan :: Chan Event -> Wai.Application
eventSourceAppUnagiChan (inChan, _outChan) req sendResponse = do
  outChan <- IO.liftIO $ Unagi.dupChan inChan
  EventSource.eventSourceAppIO (eventToServerEvent <$> Unagi.readChan outChan) req sendResponse

-- emptyEventSourcePool :: EventSourcePool
-- emptyEventSourcePool = Map.empty

-- insertIntoEventSourcePool :: Int -> EventSource -> EventSourcePool -> EventSourcePool
-- insertIntoEventSourcePool = Map.insert

-- deleteFromEventSourcePool :: Int -> EventSourcePool -> EventSourcePool
-- deleteFromEventSourcePool = Map.delete

writeEventSource :: ToSSE a => EventSource -> a -> IO ()
writeEventSource (inChan, _outChan) = Unagi.writeChan inChan . toSSE
