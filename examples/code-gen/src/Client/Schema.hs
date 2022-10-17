{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# HLINT ignore "Use camelCase" #-}

module Client.Schema where

import Data.Morpheus.Client.CodeGen.Internal
import Globals.GQLScalars

newtype Bird = Bird
  { name :: Maybe String
  }
  deriving (Generic, Show, Eq)

instance ToJSON Bird where
  toJSON (Bird birdName) =
    omitNulls
      ["name" .= birdName]

newtype Cat = Cat
  { name :: String
  }
  deriving (Generic, Show, Eq)

instance ToJSON Cat where
  toJSON (Cat catName) =
    omitNulls
      ["name" .= catName]

data CityID
  = CityIDParis
  | CityIDBLN
  | CityIDHH
  deriving (Generic, Show, Eq)

instance FromJSON CityID where
  parseJSON = \case
    "Paris" -> pure CityIDParis
    "BLN" -> pure CityIDBLN
    "HH" -> pure CityIDHH
    v -> invalidConstructorError v

instance ToJSON CityID where
  toJSON = \case
    CityIDParis -> "Paris"
    CityIDBLN -> "BLN"
    CityIDHH -> "HH"

data Coordinates = Coordinates
  { latitude :: Euro,
    longitude :: [Maybe [[UniqueID]]]
  }
  deriving (Generic, Show, Eq)

instance ToJSON Coordinates where
  toJSON (Coordinates coordinatesLatitude coordinatesLongitude) =
    omitNulls
      [ "latitude" .= coordinatesLatitude,
        "longitude" .= coordinatesLongitude
      ]

newtype Dog = Dog
  { name :: String
  }
  deriving (Generic, Show, Eq)

instance ToJSON Dog where
  toJSON (Dog dogName) =
    omitNulls
      ["name" .= dogName]

instance FromJSON Euro where
  parseJSON = scalarFromJSON

instance ToJSON Euro where
  toJSON = scalarToJSON

data Power
  = PowerShapeshifting
  | PowerThunderbolt
  | PowerLightning
  | PowerTeleportation
  | PowerOmniscience
  deriving (Generic, Show, Eq)

instance FromJSON Power where
  parseJSON = \case
    "Shapeshifting" -> pure PowerShapeshifting
    "Thunderbolt" -> pure PowerThunderbolt
    "Lightning" -> pure PowerLightning
    "Teleportation" -> pure PowerTeleportation
    "Omniscience" -> pure PowerOmniscience
    v -> invalidConstructorError v

instance ToJSON Power where
  toJSON = \case
    PowerShapeshifting -> "Shapeshifting"
    PowerThunderbolt -> "Thunderbolt"
    PowerLightning -> "Lightning"
    PowerTeleportation -> "Teleportation"
    PowerOmniscience -> "Omniscience"

data UniqueID = UniqueID
  { name :: Maybe String,
    id :: String,
    rec :: Maybe UniqueID
  }
  deriving (Generic, Show, Eq)

instance ToJSON UniqueID where
  toJSON (UniqueID uniqueIDName uniqueIDId uniqueIDRec) =
    omitNulls
      [ "name" .= uniqueIDName,
        "id" .= uniqueIDId,
        "rec" .= uniqueIDRec
      ]
