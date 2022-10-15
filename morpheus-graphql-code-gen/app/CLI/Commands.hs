module CLI.Commands where

data Operation
  = Build {source :: [FilePath]}
  | About
  deriving (Show)

data App = App
  { operations :: Operation,
    options :: Options
  }
  deriving (Show)

data Options = Options
  { version :: Bool,
    root :: String,
    namespaces :: Bool
  }
  deriving (Show)