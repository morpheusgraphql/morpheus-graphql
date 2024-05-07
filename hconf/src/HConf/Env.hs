module HConf.Env
  ( SetupEnv (..),
  )
where

data SetupEnv = SetupEnv
  { hie :: FilePath,
    hconf :: FilePath,
    stack :: FilePath
  }
