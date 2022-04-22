{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

--as TVar

-- import Control.Monad.Trans.State

-- import qualified Control.Monad.State.Class as State

import Chess
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader hiding (ask, asks)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text
import Data.Time
import GHC.Generics
import Lucid
import Lucid.Base
import Lucid.Htmx
import Okapi
import qualified SlaveThread
import Text.InterpolatedString.Perl6
import Web.FormUrlEncoded

type Okapi a = OkapiT App a

newtype App a = App {runApp :: ReaderT (TVar Env) IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (TVar Env),
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

newtype WaitPool = WaitPool {unWaitPool :: Set Text} -- todo: Add timestamp

newtype ConfirmedPool = ConfirmedPool {unConfirmedPool :: Set Text}

type MonadApp m =
  ( Monad m,
    MonadReader Env m,
    MonadIO m
  )

main :: IO ()
main = do
  print "Running chess app"
  let newEnvTVar :: IO (TVar Env)
      newEnvTVar = do
        eventSource <- Okapi.newEventSource
        newTVarIO $ Env (WaitPool mempty) (ConfirmedPool mempty) mempty eventSource

      hoistApp :: TVar Env -> App a -> IO a
      hoistApp envTVar app = runReaderT (runApp app) envTVar

  envTVar <- newEnvTVar
  slaveThreadID <- SlaveThread.fork $
    forever $ do
      threadDelay 1000000
      confirmer envTVar
      matchmaker envTVar
  Okapi.runOkapi (hoistApp envTVar) 3000 chess
  killThread slaveThreadID

-- readIORef :: IORef a -> IO a

readFromEnvTVar :: (Env -> a) -> TVar Env -> STM a
readFromEnvTVar f envTVar = do
  env <- readTVar envTVar
  pure $ f env

readWaitPool :: TVar Env -> STM WaitPool
readWaitPool = readFromEnvTVar envWaitPool

readConfirmedPool :: TVar Env -> STM ConfirmedPool
readConfirmedPool = readFromEnvTVar envConfirmedPool

readMatches :: TVar Env -> STM (Set Match)
readMatches = readFromEnvTVar envMatches

readEventSource :: TVar Env -> STM EventSource
readEventSource = readFromEnvTVar envEventSource

-- atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b

modfyEnvTVar :: (Env -> Env) -> TVar Env -> STM ()
modfyEnvTVar f envTVar = modifyTVar' envTVar f

modifyWaitPool :: (WaitPool -> WaitPool) -> TVar Env -> STM ()
modifyWaitPool f = modfyEnvTVar (\env -> env {envWaitPool = f $ envWaitPool env})

modifyConfirmedPool :: (ConfirmedPool -> ConfirmedPool) -> TVar Env -> STM ()
modifyConfirmedPool f = modfyEnvTVar (\env -> env {envConfirmedPool = f $ envConfirmedPool env})

modifyMatches :: (Set Match -> Set Match) -> TVar Env -> STM ()
modifyMatches f = modfyEnvTVar (\env -> env {envMatches = f $ envMatches env})

-- atomicWriteIORef :: IORef a -> a -> IO ()

confirmer :: TVar Env -> IO ()
confirmer = sendConfirmMessages
  where
    sendConfirmMessages :: TVar Env -> IO ()
    sendConfirmMessages envRef = do
      -- threadDelay 100000000
      waitPool <- unWaitPool <$> atomically (readWaitPool envRef)
      -- print $ "Wait pool size: " <> (show $ Set.size waitPool)
      if Set.null waitPool
        then pure () -- print "Wait pool is empty"
        else do
          -- print "Sending confirm messages"
          eventSource <- atomically $ readEventSource envRef
          -- waitPool' <- readWaitPool envRef
          forM_ (Set.toList waitPool) (sendConfirmMessage eventSource)

    sendConfirmMessage :: EventSource -> Text -> IO ()
    sendConfirmMessage eventSource playerName = Okapi.sendEvent eventSource $ Event (Just $ "confirm-" <> playerName) Nothing ""

matchmaker :: TVar Env -> IO ()
matchmaker = tryNewMatch
  where
    tryNewMatch :: TVar Env -> IO ()
    tryNewMatch envRef = do
      -- threadDelay 100000000
      -- print "Starting matchmaker"
      confirmedPool <- unConfirmedPool <$> atomically (readConfirmedPool envRef)
      -- print $ "Confirm pool size: " <> (show $ Set.size confirmedPool)
      if Set.size confirmedPool >= 2 -- at least 2 players confirmed
        then do
          -- print "Starting match"
          let p1 = Set.elemAt 0 confirmedPool -- get p1
              p2 = Set.elemAt 1 confirmedPool -- get p2
          atomically $ modifyConfirmedPool (ConfirmedPool . Set.delete p1 . unConfirmedPool) envRef -- delete p1 from pool
          atomically $ modifyConfirmedPool (ConfirmedPool . Set.delete p2 . unConfirmedPool) envRef -- delete p2 from pool
          atomically $ modifyMatches (Set.insert $ Match (p1, p2)) envRef -- add new match
          eventSource <- atomically $ readEventSource envRef
          sendStartEvent eventSource p1 p2
        else pure () -- print "Confirmed pool is less than 2"

sendStartEvent :: EventSource -> Text -> Text -> IO ()
sendStartEvent eventSource p1Name p2Name = do
  let event1 = Event (Just $ "start-" <> p1Name) Nothing $ renderBS $ toHtml startingBoard
      event2 = Event (Just $ "start-" <> p2Name) Nothing $ renderBS $ toHtml startingBoard
  Okapi.sendEvent eventSource event1
  Okapi.sendEvent eventSource event2

-- renderStrict :: ToHtml a => a -> BS.ByteString
-- renderStrict = LBS.toStrict . renderBS . toHtmlRaw

newtype Wrap a = Wrap a

instance ToHtml a => ToHtml (Wrap a) where
  toHtml (Wrap inner) = do
    doctype_
    html_ $ do
      head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        title_ "Simple Chess"
        script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
        useHtmx
        useHtmxExtension "sse"
      body_ $ do
        main_ [class_ "container mx-auto px-40 my-auto"] $ do
          toHtml inner
  toHtmlRaw = toHtml

data Home = Home

instance ToHtml Home where
  toHtml Home = do
    h1_ [] "Hyperchess"
    div_ [] $ do
      div_ [class_ "grid grid-cols-3 gap-10"] $ do
        div_ [class_ "rounded-md flex flex-col"] $ do
          h4_ "👁️ Watch Game"
          form_ [class_ "flex flex-col"] $ do
            select_ [id_ "stream"] $ do
              option_ "John ⚔️ Bob"
              option_ "Carol ⚔️ Bob"
              option_ "John ⚔️ Bob"
            button_ [type_ "submit", class_ "px-4 py-2 bg-blue-200 text-white"] "Watch Match"
        div_ [class_ "rounded-md flex flex-col"] $ do
          h4_ "♟️ Play Game"
          form_ [hxPost_ "/register", hxTarget_ "#content", class_ "flex flex-col"] $ do
            input_ [name_ "playerName", type_ "text", placeholder_ "Name"]
            button_
              [ type_ "submit",
                class_ "px-4 py-2 bg-blue-200 text-white"
              ]
              "Join Match"
        div_ [class_ "rounded-md flex flex-col"] $ do
          h4_ "🤔 How To Play"
          ul_ [] $ do
            li_ [] "Learn chess"
            li_ [] "Register"
            li_ [] "Start playing"
      div_ [id_ "content"] "Hello"
  toHtmlRaw = toHtml

missingCard_ :: Term arg result => arg -> result
missingCard_ = term "missing-card"

intToFile :: Int -> File
intToFile 1 = FileA 
intToFile 2 = FileB 
intToFile 3 = FileC 
intToFile 4 = FileD 
intToFile 5 = FileE 
intToFile 6 = FileF 
intToFile 7 = FileG 
intToFile 8 = FileH
intToFile _ = Prelude.error "No File for that Int exists" 

intToRank :: Int -> Rank
intToRank 1 = Rank1
intToRank 2 = Rank2 
intToRank 3 = Rank3 
intToRank 4 = Rank4 
intToRank 5 = Rank5 
intToRank 6 = Rank6 
intToRank 7 = Rank7 
intToRank 8 = Rank8
intToRank _ = Prelude.error "No Rank for that Int exists" 

-- positionToXY :: Position -> (Int, Int)
-- positionToXY (file, rank) = (fromEnum file, 9 - fromEnum rank)

xyToPosition :: (Int, Int) -> Position
xyToPosition (x, y) = (intToFile y, intToRank (9 - x))

instance ToHtml Piece where
  toHtml (White, Pawn) = "♙"
  toHtml (Black, Pawn) = "♟"
  toHtml (White, Knight) = "♘"
  toHtml (Black, Knight) = "♞"
  toHtml (White, Rook) = "♖"
  toHtml (Black, Rook) = "♜"
  toHtml (White, Bishop) = "♗"
  toHtml (Black, Bishop) = "♝"
  toHtml (White, Queen) = "♕"
  toHtml (Black, Queen) = "♛"
  toHtml (White, King) = "♔"
  toHtml (Black, King) = "♚"
  toHtmlRaw = toHtml

tShow :: Show a => a -> Text
tShow = pack . show
  
instance ToHtml Board where
  toHtml (Board state highlights) = do
    -- link_ [id_ "style", rel_ "stylesheet", href_ "https://the.missing.style", hxSwapOob_ "true", hxSwap_ "outerHTML"]
    div_ [class_ "grid grid-cols-8 grid-rows-8 gap-0 h-full w-full border-2 border-black"] $ do
      let blackSquareClass_ = class_ "flex items-center justify-center bg-gray-700 aspect-square"
          whiteSquareClass_ = class_ "flex items-center justify-center bg-gray-200 aspect-square"
      forM_
        ([(x, y) | x <- [1 .. 8], y <- [1 .. 8]])
        ( \(n, m) ->
            div_
              [ id_ $ tShow n <> tShow m
              , if odd n
                  then (if odd m then whiteSquareClass_ else blackSquareClass_)
                  else (if odd m then blackSquareClass_ else whiteSquareClass_)
              ]
              -- $ "♖"
              -- $ toHtml $ show n
              -- "BB"
              $ maybe "" toHtml (Map.lookup (xyToPosition (n, m)) state)
        )

  -- tr_ [] $ do
  --   th_ [] ""
  --   th_ [] "a"
  --   th_ [] "b"
  --   th_ [] "c"
  --   th_ [] "d"
  --   th_ [] "e"
  --   th_ [] "f"
  --   th_ [] "g"
  --   th_ [] "h"
  -- tr_ [] $ do
  --   th_ [] "8"
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  -- tr_ [] $ do
  --   th_ [] "7"
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  -- tr_ [] $ do
  --   th_ [] "6"
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  -- tr_ [] $ do
  --   th_ [] "5"
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  -- tr_ [] $ do
  --   th_ [] "4"
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  -- tr_ [] $ do
  --   th_ [] "3"
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  -- tr_ [] $ do
  --   th_ [] "2"
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  -- tr_ [] $ do
  --   th_ [] "1"
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  --   td_ [] ""
  toHtmlRaw = toHtml

addPlayerToWaitPool :: TVar Env -> Text -> STM ()
addPlayerToWaitPool envRef playerName = modifyWaitPool (WaitPool . Set.insert playerName . unWaitPool) envRef

addPlayerToConfirmedPool :: TVar Env -> Text -> STM ()
addPlayerToConfirmedPool envRef playerName = modifyConfirmedPool (ConfirmedPool . Set.insert playerName . unConfirmedPool) envRef

data JoinedPool = JoinedPool Text

instance ToHtml JoinedPool where
  toHtml (JoinedPool name) = do
    div_ [hxExt_ "sse", sseConnect_ "/stream"] $ do
      div_ [hxGet_ $ "/confirm?player=" <> name, hxSwap_ "outerHTML", hxTrigger_ $ "sse:confirm-" <> name] ""
      div_ [sseSwap_ ["start-" <> name], class_ "w-1/2 aspect-square container mx-auto my-10"] $ do
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
  envRef <- ask
  liftIO $ atomically $ addPlayerToWaitPool envRef playerName
  okLucid [] $ JoinedPool playerName

data Player = Player {playerName :: Text} deriving (Eq, Show, Generic, FromForm)

stream :: Okapi Result
stream = do
  get
  seg "stream"
  envRef <- ask
  eventSource <- liftIO $ atomically $ readEventSource envRef
  connectEventSource eventSource

confirm :: Okapi Result
confirm = do
  get
  Okapi.seg "confirm"
  playerName <- queryParam "player"
  envRef <- ask
  liftIO $ atomically $ modifyWaitPool (WaitPool . Set.delete playerName . unWaitPool) envRef
  liftIO $ atomically $ addPlayerToConfirmedPool envRef playerName
  noContent []

authorize = undefined

-- do
--   authHeaderValue <- Okapi.auth
--   jwtSecret <- read @Text
--   case extractToken authHeaderValue >>= verifyToken jwtSecret of
--     Nothing -> Okapi.error401 [] ""
--     Just userID -> pure userID
