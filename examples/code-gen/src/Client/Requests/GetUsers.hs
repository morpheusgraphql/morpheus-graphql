{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# HLINT ignore "Use camelCase" #-}

module Client.Requests.GetUsers where

import Client.Schema
import Data.Morpheus.Client.Internal.CodeGen

instance RequestType GetUser where
  type RequestArgs GetUser = GetUserArgs
  __name _ = "GetUser"
  __query _ = "# Query Hero with Compile time Validation\nquery GetUser($coordinates: Coordinates!) {\n  myUser: user {\n    name\n    aliasEmail: email\n    address(coordinates: $coordinates) {\n      city\n    }\n    aliasAdress: address(coordinates: $coordinates) {\n      city\n    }\n  }\n  user {\n    email\n    name\n  }\n}\n"
  __type _ = Query

data GetUser = GetUser
  { myUser :: GetUserMyUserUser,
    user :: GetUserUserUser
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUser where
  parseJSON = undefined -- TODO: should be real function

data GetUserMyUserUser = GetUserMyUserUser
  { name :: String,
    aliasEmail :: String,
    address :: GetUserMyUserAddressAddress,
    aliasAdress :: GetUserMyUserAliasAdressAddress
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUserUser where
  parseJSON = undefined -- TODO: should be real function

newtype GetUserMyUserAddressAddress = GetUserMyUserAddressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUserAddressAddress where
  parseJSON = undefined -- TODO: should be real function

newtype GetUserMyUserAliasAdressAddress = GetUserMyUserAliasAdressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUserAliasAdressAddress where
  parseJSON = undefined -- TODO: should be real function

data GetUserUserUser = GetUserUserUser
  { email :: String,
    name :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserUserUser where
  parseJSON = undefined -- TODO: should be real function

newtype GetUserArgs = GetUserArgs
  { coordinates :: Coordinates
  }
  deriving (Generic, Show, Eq)

instance ToJSON GetUserArgs where
  toJSON {-- TODO: fix me --} = undefined -- TODO: should be real function
