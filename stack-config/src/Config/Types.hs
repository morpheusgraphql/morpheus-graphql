{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | GQL Types
module Config.Types
  ( Config (..),
    PkgGroup (..),
  )
where

import Data.Aeson (FromJSON (..), Value (..))
import Data.Text (breakOn)
import Relude hiding (Undefined)

data Version
  = VBounds Text (Maybe Text)
  | VAny
  deriving
    ( Generic,
      Show
    )

parseBounds :: Text -> Version
parseBounds x =
  let (minV, maxV) = (breakOn x "-")
   in VBounds minV (Just maxV)

instance FromJSON Version where
  parseJSON (Bool True) = pure VAny
  parseJSON (String s) = pure $ parseBounds s
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
