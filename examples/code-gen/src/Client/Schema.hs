{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# HLINT ignore "Use camelCase" #-}

module Client.Schema where

import GHC.Generics (Generic)
import Globals.GQLScalars

-- TODO: FromJSONClass
-- TODO: ToJSONClass
data Coordinates = Coordinates
  { latitude :: Euro,
    longitude :: [Maybe [[UniqueID]]]
  }
  deriving (Generic, Show, Eq)

-- TODO: ToJSONClass
data CityID
  = CityIDParis
  | CityIDBLN
  | CityIDHH
  deriving (Generic, Show, Eq)

-- TODO: FromJSONClass
-- TODO: ToJSONClass
newtype Dog = Dog
  { name :: String
  }
  deriving (Generic, Show, Eq)

-- TODO: ToJSONClass
data UniqueID = UniqueID
  { name :: Maybe String,
    id :: String,
    rec :: Maybe UniqueID
  }
  deriving (Generic, Show, Eq)

-- TODO: ToJSONClass
newtype Bird = Bird
  { name :: Maybe String
  }
  deriving (Generic, Show, Eq)

-- TODO: ToJSONClass
newtype Cat = Cat
  { name :: String
  }
  deriving (Generic, Show, Eq)

-- TODO: ToJSONClass
