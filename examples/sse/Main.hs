{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay, killThread)
import Control.Monad (forever)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.ByteString.Lazy.Char8 (pack)
import Okapi
import SlaveThread (fork)

main :: IO ()
main = do
    eventSource <- newEventSource
    threadID <- fork $ forever $ do
        threadDelay 1000000
        time <- getPOSIXTime
        sendEvent eventSource $ Event Nothing Nothing $ pack $ show time
    runOkapi id 8080 (api eventSource)
    killThread threadID

type Okapi a = OkapiT IO a

api :: EventSource -> Okapi Result
api eventSource = get >> index <|> (sse eventSource)
  where
    index :: Okapi Result
    index = okFile [("Content-Type", "text/html")] "examples/sse/sse.html"

    sse :: EventSource -> Okapi Result
    sse eventSource = seg "events" >> connectEventSource eventSource
