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

-- TODO: "Bird" ToJSONClass

newtype Cat = Cat
  { name :: String
  }
  deriving (Generic, Show, Eq)

-- TODO: "Cat" ToJSONClass

data CityID
  = CityIDParis
  | CityIDBLN
  | CityIDHH
  deriving (Generic, Show, Eq)

-- TODO: "CityID" FromJSONClass

-- TODO: "CityID" ToJSONClass

data Coordinates = Coordinates
  { latitude :: Euro,
    longitude :: [Maybe [[UniqueID]]]
  }
  deriving (Generic, Show, Eq)

-- TODO: "Coordinates" ToJSONClass

newtype Dog = Dog
  { name :: String
  }
  deriving (Generic, Show, Eq)

-- TODO: "Dog" ToJSONClass

-- TODO: "Euro" FromJSONClass

-- TODO: "Euro" ToJSONClass

data UniqueID = UniqueID
  { name :: Maybe String,
    id :: String,
    rec :: Maybe UniqueID
  }
  deriving (Generic, Show, Eq)

-- TODO: "UniqueID" ToJSONClass
