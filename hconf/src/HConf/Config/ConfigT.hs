{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HConf.Config.ConfigT
  ( ConfigT (..),
    packages,
    HCEnv (..),
    save,
    run,
    runTask,
  )
where

import Control.Exception (tryJust)
import HConf.Config.Config (Config, getPackages)
import HConf.Core.Env (Env (..))
import HConf.Utils.Chalk (Color (Green), chalk)
import HConf.Utils.Class (Check (..), HConfIO (..))
import HConf.Utils.Core (Name)
import HConf.Utils.Log (Log (..), alert, label, task)
import HConf.Utils.Yaml (readYaml, writeYaml)
import Relude

data HCEnv = HCEnv
  { config :: Config,
    env :: Env,
    indention :: Int
  }

newtype ConfigT (a :: Type) = ConfigT {_runConfigT :: ReaderT HCEnv IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader HCEnv,
      MonadIO,
      MonadFail
    )

printException :: SomeException -> String
printException = show

runConfigT :: ConfigT a -> Env -> Config -> IO (Either String a)
runConfigT (ConfigT (ReaderT f)) env config = tryJust (Just . printException) (f HCEnv {indention = 0, ..})

packages :: ConfigT [Name]
packages = getPackages <$> asks config

indent :: Int -> String
indent i = replicate (i * 2) ' '

instance Log ConfigT where
  log txt = do
    i <- asks indention
    liftIO $ putStrLn $ indent i <> txt
  inside = local (\c -> c {indention = indention c + 1})

instance HConfIO ConfigT where
  eitherRead = liftIO . eitherRead
  read = liftIO . read
  write f = liftIO . write f

run :: (ToString a) => ConfigT (Maybe a) -> Env -> IO ()
run m env@Env {..} = do
  cfg <- readYaml hconf
  runConfigT (asks config >>= check >> m) env cfg >>= handle

runTask :: String -> ConfigT () -> Env -> IO ()
runTask name m = run (label name m $> Just (chalk Green "Ok"))

handle :: (ToString a) => (Log m, Monad m) => Either String (Maybe a) -> m ()
handle res = case res of
  Left x -> alert ("ERROR: " <> x)
  (Right Nothing) -> pure ()
  (Right (Just msg)) -> log (toString msg)

save :: Config -> ConfigT ()
save cfg = label "save" $ task "hconf.yaml" $ do
  ctx <- asks id
  writeYaml (hconf $ env ctx) cfg
