{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Applicative
import Control.Concurrent
import qualified Control.Concurrent.Chan.Unagi as Unagi
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar --as TVar
import Control.Monad.Extra
-- import Control.Monad.Trans.State

-- import qualified Control.Monad.State.Class as State

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader hiding (ask, asks)
import Data.ByteString.Lazy as ByteString
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text
import GHC.Generics
import Lucid
import Lucid.Base
import Lucid.Htmx
import Okapi
import Web.FormUrlEncoded
import Data.Set (Set)
import qualified Data.Set as Set

type Okapi a = OkapiT App a

newtype Match = Match (Text, Text) deriving (Ord)

instance Eq Match where
  (Match (p1, p2)) == (Match (p1', p2')) =
    p1 == p1' && p2 == p2' || p1 == p2' && p2 == p1'

type WaitPool = Set Text

data Env = Env
  { envWaitPoolTVar :: TVar WaitPool,
    envMatchesTVar :: TVar (Set Match),
    envEventSourceTVar :: TVar EventSource
  }

newtype App a = App {runApp :: ReaderT Env IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadIO
    )

type MonadApp m =
  ( Monad m,
    MonadReader Env m,
    MonadIO m
  )

main :: IO ()
main = do
  print "Running chess app"
  env <- newEnv
  Okapi.runOkapi (hoistApp env) 3000 chess

hoistApp :: Env -> App a -> IO a
hoistApp env app = runReaderT (runApp app) env

newEnv :: IO Env
newEnv = do
  envWaitPoolTVar <- newTVarIO mempty
  envMatchesTVar <- newTVarIO mempty
  eventSource <- Unagi.newChan
  envEventSourceTVar <- newTVarIO eventSource
  pure $ Env {..}

chess :: Okapi Result
chess = home <|> register <|> match -- <|> play

data Home = Home

newtype Wrap a = Wrap a

instance ToHtml a => ToHtml (Wrap a) where
  toHtml (Wrap inner) = do
    doctype_
    html_ [lang_ "", style_ "--main-font: \"Fira Sans\", system-ui; --display-font: \"Fira Sans\", system-ui;"] $ do
      head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        title_ "Simple Chess"
        link_ [rel_ "stylesheet", href_ "https://the.missing.style"]
        link_ [id_ "stylesheet-Fira-Sans", rel_ "stylesheet", href_ "https://fonts.googleapis.com/css2?family=Fira+Sans:ital,wght@0,400;0,700;1,400&amp;family=Source+Sans+3&amp;display=swap"]
        useHtmx
        useHtmxExtension "sse"
      body_ $ do
        main_ [] $ do
          toHtml inner
  toHtmlRaw = toHtml

instance ToHtml Home where
  toHtml Home = do
    h1_ [] "Hyperchess"
    div_ [id_ "content"] $ do
      ul_ [role_ "list", class_ "basicgrid crowded", style_ "--col-width: 15ch"] $ do
        li_ [class_ "missing-card"] $ do
          h4_ "👁️ Watch Game"
          form_ [] $ do
            fieldset_ [id_ "forms__input"] $ do
              legend_ [] "Ongoing Matches"
              div_ [class_ "table rows"] $ do
                p_ [] $ do
                  select_ [id_ "match"] $ do
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
  toHtmlRaw = toHtml

is_ = makeAttribute "is"

missingCard_ :: Term arg result => arg -> result
missingCard_ = term "missing-card"

home :: Okapi Result
home = do
  get
  okLucid [] $ Wrap Home

data Player = Player {playerName :: Text} deriving (Eq, Show, Generic, FromForm)

register :: Okapi Result
register = do
  Okapi.post
  Okapi.seg "register"
  Player {..} <- bodyForm @Player
  waitPoolTVar <- asks envWaitPoolTVar
  liftIO $ addPlayerToWaitPool waitPoolTVar playerName
  okLucid [] $ Joined playerName

addPlayerToWaitPool :: TVar WaitPool -> Text -> IO ()
addPlayerToWaitPool waitPoolTVar name = atomically $ modifyTVar' waitPoolTVar $ Set.insert name

tryToMakeNewMatch :: TVar WaitPool -> TVar (Set Match) -> Text -> IO Bool
tryToMakeNewMatch waitPoolTVar matchesTVar name = do
  waitPool <- atomically . readTVar $ waitPoolTVar
  print $ "Current player pool: " <> show waitPool
  atomically $ do
    -- waitPool <- readTVar waitPoolTVar
    if Set.size waitPool >= 2 -- at least 2 players
      then do
        modifyTVar' waitPoolTVar $ Set.delete name -- delete p1 from pool
        waitPool' <- readTVar waitPoolTVar -- get pool without p1
        let opponent = Set.elemAt 0 waitPool' -- get p2
        modifyTVar' waitPoolTVar $ Set.delete opponent -- delete p2 from pool
        modifyTVar' matchesTVar $ Set.insert $ Match (name, opponent) -- add new match
        pure True
      else do
        pure False -- return nothing

writeEventSource :: TVar EventSource -> Event -> IO ()
writeEventSource eventSourceTVar event = do
  (eventSourceIn, _eventSourceOut) <- liftIO . readTVarIO $ eventSourceTVar
  liftIO $ Unagi.writeChan eventSourceIn event

data Joined = Joined Text

sseConnect_ = makeAttribute "sse-connect"

sseSwap_ messageNames = makeAttribute "sse-swap" $ Data.Text.intercalate "," messageNames

instance ToHtml Joined where
  toHtml (Joined name) = do
    div_ [hxExt_ "sse", sseConnect_ $ "/match?player=" <> name, sseSwap_ ["start-" <> name, "looking-" <> name], hxTarget_ "#content"] $ do
      h4_ $ toHtml $ "Hello, " <> name <> ". I'm finding you a worthy opponent..."
  toHtmlRaw = toHtml

match :: Okapi Result
match = do
  get
  seg "match"
  playerName <- queryParam "player"
  eventSourceTVar <- asks envEventSourceTVar
  waitPoolTVar <- asks envWaitPoolTVar
  matchesTVar <- asks envMatchesTVar
  let
    succeed :: IO ()
    succeed = do
      -- print "Sending start event"
      let event = Event (Just $ "start-" <> playerName) Nothing "<h1>Match Started</h1>"
      writeEventSource eventSourceTVar event
    loop :: IO ()
    loop = do
      -- liftIO $ print "Sending looking event"
      let event = Event (Just $ "looking-" <> playerName) Nothing "<h1>Looking</h1>"
      writeEventSource eventSourceTVar event
      ifM (tryToMakeNewMatch waitPoolTVar matchesTVar playerName) succeed loop
  liftIO $ forkIO $
    ifM (tryToMakeNewMatch waitPoolTVar matchesTVar playerName)
      succeed
      loop
  eventSource <- liftIO . readTVarIO $ eventSourceTVar
  connectEventSource eventSource

play :: Okapi Result
play = do
  Okapi.seg "play"
  playerID <- segParam @Int
  ok [] ""

authorize = undefined --do
--   authHeaderValue <- Okapi.auth
--   jwtSecret <- grab @Text
--   case extractToken authHeaderValue >>= verifyToken jwtSecret of
--     Nothing -> Okapi.error401 [] ""
--     Just userID -> pure userID
