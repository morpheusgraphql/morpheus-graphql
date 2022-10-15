module CLI.Config (App (..), Config (..)) where

import Data.Text (Text)

data App = App
  { name :: Text,
    root :: Text,
    path :: Text,
    options :: Text
  }

data Config = Config
  { server :: App,
    client :: App
  }
