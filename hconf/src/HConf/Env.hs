module HConf.Env
  ( Env (..),
  )
where

data Env = Env
  { hie :: FilePath,
    hconf :: FilePath,
    stack :: FilePath
  }
