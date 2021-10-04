module Lib where

-- import           Data.Typeable (Typeable)
-- routes
--   :: ( Typeable effs
--      , Member OrganizationRepo effs
--      , LastMember IO effs )
--   => ScottyT Text (Eff effs) ()
-- routes = do
--     Organization.routes
--     otherRoutes

-- server :: Bool -> IO () -> IO ()
-- server showStart readyAction = do
--     db <- organizationDBRef
--     scottyOptsT  (Options showStartMessage settings) (persistentOrgRepoHandler db) routes
--   where
--     settings =
--         setBeforeMainLoop readyAction $
--             setPort 8080
--             defaultSettings
--     showStartMessage = if showStart then 1 else 0

-- webServer :: IO ()
-- webServer = server True (pure ())

