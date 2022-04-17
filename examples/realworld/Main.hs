{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Conduit.Database
import Conduit.Server
import Conduit.Type
import Control.Monad.Reader (runReaderT)
import qualified Hasql.Connection as Connection
import qualified Okapi
import System.Random
import qualified Data.Text as Text

main :: IO ()
main = do
  config <- getConfig
  setupDB $ configDBConnection config
  Okapi.runOkapi (hoistHandler config) 3000 conduit

hoistHandler :: Config -> Handler a -> IO a
hoistHandler config app = runReaderT (runHandler app) config

getConfig :: IO Config
getConfig = do
  let dbConnSettings = Connection.settings
        "localhost"
        5432
        "realworld"
        "abc"
        "realworld_api"
  connResult <- Connection.acquire dbConnSettings

  case connResult of
    Left err -> error $ show err
    Right configDBConnection -> do
      gen <- newStdGen
      let configJWTSecret = Text.pack $ take 50 $ randoms gen
      print "Config generated successfully"
      pure $ Config {..}
