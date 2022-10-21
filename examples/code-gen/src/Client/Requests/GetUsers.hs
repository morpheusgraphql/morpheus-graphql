{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Client.Requests.GetUsers where

import Client.Schema
import Data.Morpheus.Client.CodeGen.Internal

instance RequestType GetUser where
  type RequestArgs GetUser = GetUserArgs
  __name _ = "GetUser"
  __query _ = "# Query Hero with Compile time Validation\nquery GetUser($coordinates: Coordinates!) {\n  myUser: user {\n    name\n    aliasEmail: email\n    address(coordinates: $coordinates) {\n      city\n    }\n    aliasAdress: address(coordinates: $coordinates) {\n      city\n    }\n  }\n  user {\n    email\n    name\n    entity {\n      ... on User {\n        __typename\n        name\n      }\n    }\n  }\n  character {\n    ... on Deity {\n      __typename\n      power\n    }\n  }\n}\n"
  __type _ = Query

data GetUser = GetUser
  { myUser :: GetUserMyUser,
    user :: GetUserUser,
    character :: [GetUserCharacter]
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUser where
  parseJSON =
    withObject "GetUser" (\v -> GetUser <$> v .: "myUser" <*> v .: "user" <*> v .: "character")

data GetUserMyUser = GetUserMyUser
  { name :: String,
    aliasEmail :: String,
    address :: GetUserMyUserAddress,
    aliasAdress :: GetUserMyUserAliasAdress
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUser where
  parseJSON =
    withObject "GetUserMyUser" (\v -> GetUserMyUser <$> v .: "name" <*> v .: "aliasEmail" <*> v .: "address" <*> v .: "aliasAdress")

newtype GetUserMyUserAddress = GetUserMyUserAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUserAddress where
  parseJSON =
    withObject "GetUserMyUserAddress" (\v -> GetUserMyUserAddress <$> v .: "city")

newtype GetUserMyUserAliasAdress = GetUserMyUserAliasAdress
  { city :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUserAliasAdress where
  parseJSON =
    withObject "GetUserMyUserAliasAdress" (\v -> GetUserMyUserAliasAdress <$> v .: "city")

data GetUserUser = GetUserUser
  { email :: String,
    name :: String,
    entity :: [GetUserUserEntity]
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserUser where
  parseJSON =
    withObject "GetUserUser" (\v -> GetUserUser <$> v .: "email" <*> v .: "name" <*> v .: "entity")

data GetUserUserEntity
  = GetUserUserEntityVariantUser GetUserUserEntityUser
  | GetUserUserEntity
  deriving (Generic, Show, Eq)

instance FromJSON GetUserUserEntity where
  parseJSON =
    withUnion
      ( \case
          ("User", v) -> GetUserUserEntityVariantUser <$> parseJSON v
          (_fallback, _) -> pure GetUserUserEntity
      )

data GetUserUserEntityUser = GetUserUserEntityUser
  { __typename :: String,
    name :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserUserEntityUser where
  parseJSON =
    withObject "GetUserUserEntityUser" (\v -> GetUserUserEntityUser <$> v .: "__typename" <*> v .: "name")

data GetUserCharacter
  = GetUserCharacterVariantDeity GetUserCharacterDeity
  | GetUserCharacter
  deriving (Generic, Show, Eq)

instance FromJSON GetUserCharacter where
  parseJSON =
    withUnion
      ( \case
          ("Deity", v) -> GetUserCharacterVariantDeity <$> parseJSON v
          (_fallback, _) -> pure GetUserCharacter
      )

data GetUserCharacterDeity = GetUserCharacterDeity
  { __typename :: String,
    power :: Power
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserCharacterDeity where
  parseJSON =
    withObject "GetUserCharacterDeity" (\v -> GetUserCharacterDeity <$> v .: "__typename" <*> v .: "power")

newtype GetUserArgs = GetUserArgs
  { coordinates :: Coordinates
  }
  deriving (Generic, Show, Eq)

instance ToJSON GetUserArgs where
  toJSON (GetUserArgs getUserArgsCoordinates) =
    omitNulls
      [ "coordinates" .= getUserArgsCoordinates
      ]
