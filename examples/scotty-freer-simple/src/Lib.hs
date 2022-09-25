{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Lib
  ( server,
  )
where

import Control.Monad.Freer (Eff, LastMember, Member)
import Data.Text.Lazy (Text)
import Data.Typeable (Typeable)
import DeityRepo (DeityRepo)
import ExampleDeityRepoHandler (deityIORef, exampleDeityRepoHandler)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    setBeforeMainLoop,
    setPort,
  )
import qualified Scotty as Dieties (routes)
import Web.Scotty (Options (Options))
import Web.Scotty.Trans (ScottyT, scottyOptsT)

server :: IO ()
server = server' True (pure ())

server' :: Bool -> IO () -> IO ()
server' showStart readyAction = do
  deityIORef' <- deityIORef
  scottyOptsT
    (Options showStartMessage settings)
    (exampleDeityRepoHandler deityIORef')
    routes
  where
    settings =
      setBeforeMainLoop readyAction $
        setPort
          8080
          defaultSettings
    showStartMessage = if showStart then 1 else 0

routes ::
  ( Typeable effs,
    Member DeityRepo effs,
    LastMember IO effs
  ) =>
  ScottyT Text (Eff effs) ()
routes = do
  Dieties.routes
