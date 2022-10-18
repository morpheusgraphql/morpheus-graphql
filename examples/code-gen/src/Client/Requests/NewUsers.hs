{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# HLINT ignore "Use camelCase" #-}

module Client.Requests.NewUsers where

import Client.Schema
import Data.Morpheus.Client.CodeGen.Internal

instance RequestType NewUsers where
  type RequestArgs NewUsers = NewUsersArgs
  __name _ = "NewUsers"
  __query _ = "subscription NewUsers($loc: Coordinates!) {\n  newUser {\n    name\n    email\n    address(coordinates: $loc) {\n      city\n    }\n    worships {\n      ... on Hero {\n        hobby\n      }\n    }\n  }\n}\n"
  __type _ = Subscription

newtype NewUsers = NewUsers
  { newUser :: NewUsersNewUserUser
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewUsers where
  parseJSON = withObject "NewUsers" (\v -> NewUsers <$> v .: "newUser")

data NewUsersNewUserUser = NewUsersNewUserUser
  { name :: String,
    email :: String,
    address :: NewUsersNewUserAddressAddress,
    worships :: NewUsersNewUserWorshipsCharacter
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewUsersNewUserUser where
  parseJSON = withObject "NewUsersNewUserUser" (\v -> NewUsersNewUserUser <$> v .: "name" <*> v .: "email" <*> v .: "address" <*> v .: "worships")

newtype NewUsersNewUserAddressAddress = NewUsersNewUserAddressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewUsersNewUserAddressAddress where
  parseJSON = withObject "NewUsersNewUserAddressAddress" (\v -> NewUsersNewUserAddressAddress <$> v .: "city")

data NewUsersNewUserWorshipsCharacter
  = NewUsersNewUserWorshipsHero
      { hobby :: String
      }
  | NewUsersNewUserWorshipsCharacter
  deriving (Generic, Show, Eq)

instance FromJSON NewUsersNewUserWorshipsCharacter where
  parseJSON =
    takeValueType
      ( \case
          ("Hero", v) -> NewUsersNewUserWorshipsHero <$> v .: "hobby"
          (_fallback, _) -> pure NewUsersNewUserWorshipsCharacter
      )

newtype NewUsersArgs = NewUsersArgs
  { loc :: Coordinates
  }
  deriving (Generic, Show, Eq)

instance ToJSON NewUsersArgs where
  toJSON (NewUsersArgs newUsersArgsLoc) =
    omitNulls
      ["loc" .= newUsersArgsLoc]
