module HConf.Core.Env
  ( Env (..),
  )
where

data Env = Env
  { hie :: FilePath,
    hconf :: FilePath,
    stack :: FilePath,
    silence :: Bool
  }
