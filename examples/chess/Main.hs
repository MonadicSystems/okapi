{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

--as TVar

-- import Control.Monad.Trans.State

-- import qualified Control.Monad.State.Class as State

import Chess
import Control.Applicative
import Control.Concurrent
import qualified Control.Concurrent.Chan.Unagi as Unagi
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader hiding (ask, asks)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text
import GHC.Generics
import Lucid
import Lucid.Base
import Lucid.Htmx
import Okapi
import Text.InterpolatedString.Perl6
import Web.FormUrlEncoded
import Data.IORef

type Okapi a = OkapiT App a

newtype App a = App {runApp :: ReaderT (IORef Env) IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (IORef Env),
      MonadIO
    )

data Env = Env
  { envWaitPool :: WaitPool,
    envConfirmedPool :: ConfirmedPool,
    envMatches :: Set Match,
    envEventSource :: EventSource
  }

newtype Match = Match (Text, Text) deriving (Show, Ord)

instance Eq Match where
  (Match (p1, p2)) == (Match (p1', p2')) =
    p1 == p1' && p2 == p2' || p1 == p2' && p2 == p1'

type WaitPool = Set Text

type ConfirmedPool = Set Text

type MonadApp m =
  ( Monad m,
    MonadReader Env m,
    MonadIO m
  )

main :: IO ()
main = do
  print "Running chess app"
  let
    newEnv :: IO (IORef Env)
    newEnv = do
      eventSource <- Unagi.newChan
      newIORef $ Env mempty mempty mempty eventSource

    hoistApp :: IORef Env -> App a -> IO a
    hoistApp env app = runReaderT (runApp app) env

  env <- newEnv
  forkIO $ confirmer env
  forkIO $ matchmaker env
  Okapi.runOkapi (hoistApp env) 3000 chess

confirmer :: IORef Env -> IO ()
confirmer env = do
  let waitPoolTVar = envWaitPoolTVar env
      matchesTVar = envMatchesTVar env
      eventSourceTVar = envEventSourceTVar env
  forever $ sendConfirmMessages waitPoolTVar eventSourceTVar
  where
    sendConfirmMessages :: TVar WaitPool -> TVar EventSource -> IO ()
    sendConfirmMessages waitPoolTVar eventSourceTVar = do
      threadDelay 100000000
      waitPool <- readTVarIO waitPoolTVar
      eventSource <- readTVarIO eventSourceTVar
      if Set.null waitPool
        then pure ()
        else forM_ (Set.toList waitPool) (sendConfirmMessage eventSource)

    sendConfirmMessage :: EventSource -> Text -> IO ()
    sendConfirmMessage eventSource playerName = sendEvent eventSource $ Event (Just $ "confirm-" <> playerName) Nothing ""

matchmaker :: IORef Env -> IO ()
matchmaker env = do
  let confirmedPoolTVar = envConfirmedPoolTVar env
      matchesTVar = envMatchesTVar env
      eventSourceTVar = envEventSourceTVar env
  forever $ tryNewMatch confirmedPoolTVar matchesTVar eventSourceTVar
  where
    tryNewMatch :: TVar ConfirmedPool -> TVar (Set Match) -> TVar EventSource -> IO ()
    tryNewMatch confirmedPoolTVar matchesTVar eventSourceTVar = do
      threadDelay 100000000
      confirmedPool <- readTVarIO confirmedPoolTVar
      eventSource <- readTVarIO eventSourceTVar
      if Set.size confirmedPool >= 2 -- at least 2 players confirmed
        then do
          let p1 = Set.elemAt 0 confirmedPool -- get p1
              p2 = Set.elemAt 1 confirmedPool -- get p2
          atomically $ modifyTVar' confirmedPoolTVar $ Set.delete p1 -- delete p1 from pool
          atomically $ modifyTVar' confirmedPoolTVar $ Set.delete p2 -- delete p2 from pool
          atomically $ modifyTVar' matchesTVar $ Set.insert $ Match (p1, p2) -- add new match
          sendStartEvent eventSource p1 p2
        else pure ()

sendStartEvent :: EventSource -> Text -> Text -> IO ()
sendStartEvent eventSource p1Name p2Name = do
  let event1 = Event (Just $ "start-" <> p1Name) Nothing $ renderStrict startingBoard
      event2 = Event (Just $ "start-" <> p2Name) Nothing $ renderStrict startingBoard
  sendEvent eventSource event1
  sendEvent eventSource event2

sendEvent :: EventSource -> Event -> IO ()
sendEvent eventSource event = do
  let (eventSourceIn, _eventSourceOut) = eventSource
  liftIO $ Unagi.writeChan eventSourceIn event

renderStrict :: ToHtml a => a -> BS.ByteString
renderStrict = LBS.toStrict . renderBS . toHtmlRaw

newtype Wrap a = Wrap a

instance ToHtml a => ToHtml (Wrap a) where
  toHtml (Wrap inner) = do
    doctype_
    html_ [lang_ "", style_ "--main-font: \"Fira Sans\", system-ui; --display-font: \"Fira Sans\", system-ui;"] $ do
      head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        title_ "Simple Chess"
        link_ [id_ "style", rel_ "stylesheet", href_ "https://the.missing.style"]
        link_ [id_ "stylesheet-Fira-Sans", rel_ "stylesheet", href_ "https://fonts.googleapis.com/css2?family=Fira+Sans:ital,wght@0,400;0,700;1,400&amp;family=Source+Sans+3&amp;display=swap"]
        useHtmx
        useHtmxExtension "sse"
      body_ $ do
        main_ [] $ do
          toHtml inner
  toHtmlRaw = toHtml

data Home = Home

instance ToHtml Home where
  toHtml Home = do
    h1_ [] "Hyperchess"
    div_ [] $ do
      ul_ [role_ "list", class_ "basicgrid crowded", style_ "--col-width: 15ch"] $ do
        li_ [class_ "missing-card"] $ do
          h4_ "👁️ Watch Game"
          form_ [] $ do
            fieldset_ [id_ "forms__input"] $ do
              legend_ [] "Ongoing Matches"
              div_ [class_ "table rows"] $ do
                p_ [] $ do
                  select_ [id_ "stream"] $ do
                    option_ "John ⚔️ Bob"
                    option_ "Carol ⚔️ Bob"
                    option_ "John ⚔️ Bob"
                  input_ [type_ "submit", value_ "Watch Match", style_ "margin-top: 2ch"]
        li_ [class_ "missing-card"] $ do
          h4_ "♟️ Play Game"
          form_ [hxPost_ "/register", hxTarget_ "#content"] $ do
            fieldset_ [id_ "forms__input"] $ do
              legend_ [] "Register"
              div_ [class_ "table rows"] $ do
                p_ [] $ do
                  input_ [name_ "playerName", type_ "text", placeholder_ "Name"]
                  button_
                    [ style_ "margin-top: 2ch",
                      type_ "submit"
                    ]
                    "Join Match"
          li_ [class_ "missing-card"] $ do
            h4_ "🤔 How To Play"
            video_ [controls_ ""] ""
      div_ [id_ "content"] "Hello"
  toHtmlRaw = toHtml

missingCard_ :: Term arg result => arg -> result
missingCard_ = term "missing-card"

instance ToHtml Board where
  toHtml (Board state highlights) = do
    -- link_ [id_ "style", rel_ "stylesheet", href_ "https://the.missing.style", hxSwapOob_ "true", hxSwap_ "outerHTML"]
    table_ [class_ "board"] $ do
      tr_ [] $ do
        th_ [] ""
        th_ [] "a"
        th_ [] "b"
        th_ [] "c"
        th_ [] "d"
        th_ [] "e"
        th_ [] "f"
        th_ [] "g"
        th_ [] "h"
      tr_ [] $ do
        th_ [] "8"
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
      tr_ [] $ do
        th_ [] "7"
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
      tr_ [] $ do
        th_ [] "6"
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
      tr_ [] $ do
        th_ [] "5"
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
      tr_ [] $ do
        th_ [] "4"
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
      tr_ [] $ do
        th_ [] "3"
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
      tr_ [] $ do
        th_ [] "2"
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
      tr_ [] $ do
        th_ [] "1"
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
        td_ [] ""
  toHtmlRaw = toHtml

addPlayerToWaitPool :: TVar WaitPool -> Text -> IO ()
addPlayerToWaitPool waitPoolTVar name = atomically $ modifyTVar' waitPoolTVar $ Set.insert name

addPlayerToConfirmedPool :: TVar ConfirmedPool -> Text -> IO ()
addPlayerToConfirmedPool confirmedPoolTVar name = atomically $ modifyTVar' confirmedPoolTVar $ Set.insert name

data JoinedPool = JoinedPool Text

instance ToHtml JoinedPool where
  toHtml (JoinedPool name) = do
    div_ [hxExt_ "sse", sseConnect_ "/stream"] $ do
      div_ [hxGet_ $ "/confirm?" <> name, hxSwap_ "outerHTML", hxTrigger_ $ "sse:confirm-" <> name] ""
      div_ [sseSwap_ ["start-" <> name]] $ do
        h4_ $ toHtml $ "Hello, " <> name <> ". Finding an opponent..."
  toHtmlRaw = toHtml

-- data Confirmed = Confirmed Text

-- instance ToHtml Confirmed where
--   toHtml (Confirmed name) = do
--     div_ [sseSwap_ ["start-"<>name]] $ do
--       h4_ $ toHtml $ ("Connection confirmed. I'm finding you an opponent now..." :: Text)
--   toHtmlRaw = toHtml

sseConnect_ :: Text -> Attribute
sseConnect_ = makeAttribute "sse-connect"

sseSwap_ :: [Text] -> Attribute
sseSwap_ messageNames = makeAttribute "sse-swap" $ Data.Text.intercalate "," messageNames

chess :: Okapi Result
chess = home <|> register <|> stream <|> confirm

home :: Okapi Result
home = do
  get
  okLucid [] $ Wrap Home

register :: Okapi Result
register = do
  Okapi.post
  Okapi.seg "register"
  Player {..} <- bodyForm
  waitPoolTVar <- asks envWaitPoolTVar
  liftIO $ addPlayerToWaitPool waitPoolTVar playerName
  okLucid [] $ JoinedPool playerName

data Player = Player {playerName :: Text} deriving (Eq, Show, Generic, FromForm)

stream :: Okapi Result
stream = do
  get
  seg "stream"
  eventSourceTVar <- asks envEventSourceTVar
  eventSource <- liftIO . readTVarIO $ eventSourceTVar
  connectEventSource eventSource

confirm :: Okapi Result
confirm = do
  Okapi.seg "confirm"
  playerName <- queryParam "player"
  confirmedPoolTVar <- asks envConfirmedPoolTVar
  waitPoolTVar <- asks envWaitPoolTVar
  liftIO $ atomically $ modifyTVar' waitPoolTVar $ Set.delete playerName
  liftIO $ addPlayerToConfirmedPool confirmedPoolTVar playerName
  noContent []

authorize = undefined

-- do
--   authHeaderValue <- Okapi.auth
--   jwtSecret <- grab @Text
--   case extractToken authHeaderValue >>= verifyToken jwtSecret of
--     Nothing -> Okapi.error401 [] ""
--     Just userID -> pure userID
