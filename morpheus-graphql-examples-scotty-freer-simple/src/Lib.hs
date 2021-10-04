{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (
    -- server,
) where

-- import           Control.Monad.Freer                              (Eff,
--                                                                    LastMember,
--                                                                    Member)
-- import           Data.Text.Lazy                                   (Text)
-- import           Data.Typeable                                    (Typeable)
-- import           Network.Wai.Handler.Warp                         (defaultSettings,
--                                                                    setBeforeMainLoop,
--                                                                    setPort)
-- import           Web.Scotty                                       (Options (Options))
-- import           Web.Scotty.Trans                                 (ScottyT, get,
--                                                                    html, param,
--                                                                    scottyOptsT)

-- server :: IO ()
-- server = server' True (pure ())

-- server' :: Bool -> IO () -> IO ()
-- server' showStart readyAction = do
--     db <- organizationDBRef
--     scottyOptsT  (Options showStartMessage settings) handlers routes
--   where
--     settings =
--         setBeforeMainLoop readyAction $
--             setPort 8080
--             defaultSettings
--     showStartMessage = if showStart then 1 else 0

-- handlers = undefined



