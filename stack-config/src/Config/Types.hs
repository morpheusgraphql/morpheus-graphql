{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Config.Types
  ( Config (..),
    PkgGroup (..),
  )
where

import Data.Aeson (FromJSON)
import Relude hiding (Undefined)

data PkgGroup = PkgGroup
  { dir :: Text,
    include :: [Text],
    prefix :: Maybe Text
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

data Build = Build
  { resolver :: Text,
    extra :: Map Text Text,
    -- include :: [Text],
    skip :: Maybe [Text]
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )

data Config = Config
  { name :: Text,
    version :: Text,
    bounds :: Text,
    packages :: [PkgGroup],
    builds :: Map Text Build
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )
