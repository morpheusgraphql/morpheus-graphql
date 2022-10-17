{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# HLINT ignore "Use camelCase" #-}

module Client.Requests.GetUsers where

import Client.Schema
import Data.Morpheus.Client.CodeGen.Internal

instance RequestType GetUser where
  type RequestArgs GetUser = GetUserArgs
  __name _ = "GetUser"
  __query _ = "# Query Hero with Compile time Validation\nquery GetUser($coordinates: Coordinates!) {\n  myUser: user {\n    name\n    aliasEmail: email\n    address(coordinates: $coordinates) {\n      city\n    }\n    aliasAdress: address(coordinates: $coordinates) {\n      city\n    }\n  }\n  user {\n    email\n    name\n  }\n\n  character {\n    ... on Deity {\n      power\n    }\n  }\n}\n"
  __type _ = Query

data GetUser = GetUser
  { myUser :: GetUserMyUserUser,
    user :: GetUserUserUser,
    character :: [GetUserCharacterCharacter]
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUser where
  parseJSON = withObject "GetUser" (\v -> GetUser <$> v .: "myUser" <*> v .: "user" <*> v .: "character")

data GetUserMyUserUser = GetUserMyUserUser
  { name :: String,
    aliasEmail :: String,
    address :: GetUserMyUserAddressAddress,
    aliasAdress :: GetUserMyUserAliasAdressAddress
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUserUser where
  parseJSON = withObject "GetUserMyUserUser" (\v -> GetUserMyUserUser <$> v .: "name" <*> v .: "aliasEmail" <*> v .: "address" <*> v .: "aliasAdress")

newtype GetUserMyUserAddressAddress = GetUserMyUserAddressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUserAddressAddress where
  parseJSON = withObject "GetUserMyUserAddressAddress" (\v -> GetUserMyUserAddressAddress <$> v .: "city")

newtype GetUserMyUserAliasAdressAddress = GetUserMyUserAliasAdressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUserAliasAdressAddress where
  parseJSON = withObject "GetUserMyUserAliasAdressAddress" (\v -> GetUserMyUserAliasAdressAddress <$> v .: "city")

data GetUserUserUser = GetUserUserUser
  { email :: String,
    name :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserUserUser where
  parseJSON = withObject "GetUserUserUser" (\v -> GetUserUserUser <$> v .: "email" <*> v .: "name")

data GetUserCharacterCharacter
  = GetUserCharacterCharacter
      { __typename :: String
      }
  | GetUserCharacterDeity
      { __typename :: String,
        power :: Power
      }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserCharacterCharacter where
  parseJSON =
    takeValueType
      ( \case
          ("Character", v) -> GetUserCharacterCharacter <$> v .: "__typename"
          ("Deity", v) -> GetUserCharacterDeity <$> v .: "__typename" <*> v .: "power"
          (_, v) -> GetUserCharacterCharacter <$> v .: "__typename"
      )

newtype GetUserArgs = GetUserArgs
  { coordinates :: Coordinates
  }
  deriving (Generic, Show, Eq)

instance ToJSON GetUserArgs where
  toJSON (GetUserArgs getUserArgsCoordinates) =
    omitNulls
      ["coordinates" .= getUserArgsCoordinates]
