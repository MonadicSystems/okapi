{-# LANGUAGE RankNTypes #-}

module Okapi.Test where
import Okapi
import qualified Network.Wai as Wai
import qualified Control.Monad.Morph as Morph
import qualified Control.Monad.Except as ExceptT
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State.Strict as StateT
import Okapi.State
import qualified Network.Wai.Test as Wai.Test

runSession :: Monad m => Wai.Test.Session a -> (forall a. m a -> IO a) -> OkapiT m Response -> IO a
runSession session hoister okapiT = do
    let app = makeOkapiApp hoister notFound okapiT
    Wai.Test.runSession session app

withSession :: Monad m => (forall a. m a -> IO a) -> OkapiT m Response -> Wai.Test.Session a -> IO a
withSession hoister okapiT session = runSession session hoister okapiT
