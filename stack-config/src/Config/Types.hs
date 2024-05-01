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
    include :: Text
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
    packages :: [PkgGroup]
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )
