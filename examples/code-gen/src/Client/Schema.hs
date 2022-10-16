{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# HLINT ignore "Use camelCase" #-}

module Client.Schema where

import GHC.Generics (Generic)
import Globals.GQLScalars

newtype Bird = Bird
  { name :: Maybe String
  }
  deriving (Generic, Show, Eq)

instance ToJSON Bird where
  toJSON _ = undefined

newtype Cat = Cat
  { name :: String
  }
  deriving (Generic, Show, Eq)

instance ToJSON Cat where
  toJSON _ = undefined

data CityID
  = CityIDParis
  | CityIDBLN
  | CityIDHH
  deriving (Generic, Show, Eq)

instance FromJSON CityID where
  parseJSON _ = undefined

instance ToJSON CityID where
  toJSON _ = undefined

data Coordinates = Coordinates
  { latitude :: Euro,
    longitude :: [Maybe [[UniqueID]]]
  }
  deriving (Generic, Show, Eq)

instance ToJSON Coordinates where
  toJSON _ = undefined

newtype Dog = Dog
  { name :: String
  }
  deriving (Generic, Show, Eq)

instance ToJSON Dog where
  toJSON _ = undefined

instance FromJSON Euro where
  parseJSON _ = undefined

instance ToJSON Euro where
  toJSON _ = undefined

data UniqueID = UniqueID
  { name :: Maybe String,
    id :: String,
    rec :: Maybe UniqueID
  }
  deriving (Generic, Show, Eq)

instance ToJSON UniqueID where
  toJSON _ = undefined
