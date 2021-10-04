{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (
    server,
) where

import           Control.Monad.Freer                              (Eff,
                                                                   LastMember,
                                                                   Member)
import           Data.Text.Lazy                                   (Text)
import           Data.Typeable                                    (Typeable)
import           Network.Wai.Handler.Warp                         (defaultSettings,
                                                                   setBeforeMainLoop,
                                                                   setPort)
import           Web.Scotty                                       (Options (Options))
import           Web.Scotty.Trans                                 (ScottyT, get,
                                                                   html, param,
                                                                   scottyOptsT)
import qualified Scotty as Dieties (routes) 
import ExampleDeityRepoHandler (exampleDeityRepoHandler)
import DeityRepo
import Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class                        (lift)

server :: IO ()
server = server' True (pure ())

server' :: Bool -> IO () -> IO ()
server' showStart readyAction = do
    scottyOptsT  (Options showStartMessage settings) handlers routes
  where
    settings =
        setBeforeMainLoop readyAction $
            setPort 8080
            defaultSettings
    showStartMessage = if showStart then 1 else 0
    handlers = liftIO exampleDeityRepoHandler

routes
  :: ( Typeable effs
     , Member DeityRepo effs
     , LastMember IO effs )
  => ScottyT Text (Eff effs) ()
routes = do
    Dieties.routes



