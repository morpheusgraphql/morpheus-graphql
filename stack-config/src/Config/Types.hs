{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Config.Types
  ( Config (..),
    PkgGroup (..),
  )
where

import Data.Aeson (FromJSON (..), Value (..))
import Relude hiding (Undefined)

data Version
  = VBounds String (Maybe String)
  | VAny
  deriving
    ( Generic,
      Show
    )

instance FromJSON Version where
  parseJSON (Bool True) = pure VAny
  parseJSON (String s) = pure $ (VBounds (show s)) Nothing
  parseJSON (Number n) = pure $ VBounds (show n) Nothing
  parseJSON v = fail $ "version should be either true or string" <> (show v)

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

type Deps = Map Text Version

data Build = Build
  { resolver :: Text,
    extra :: Maybe Deps,
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
    builds :: Map Text Build,
    dependencies :: Deps
  }
  deriving
    ( Generic,
      FromJSON,
      Show
    )
