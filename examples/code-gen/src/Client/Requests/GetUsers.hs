{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeFamilies #-}

{-# HLINT ignore "Use camelCase" #-}

module Client.Requests.GetUsers where

import Client.Schema
import Data.Morpheus.Client.Internal.CodeGen

instance RequestType GetUser where
  type RequestArgs GetUser = GetUserArgs
  __name _ = undefined -- TODO: should be real function
  __query _ = undefined -- TODO: should be real function
  __type _ = undefined -- TODO: should be real function

data GetUser = GetUser
  { myUser :: GetUserMyUserUser,
    user :: GetUserUserUser
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUser where
  parseJSON _ = undefined -- TODO: should be real function

data GetUserMyUserUser = GetUserMyUserUser
  { name :: String,
    aliasEmail :: String,
    address :: GetUserMyUserAddressAddress,
    aliasAdress :: GetUserMyUserAliasAdressAddress
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUserUser where
  parseJSON _ = undefined -- TODO: should be real function

newtype GetUserMyUserAddressAddress = GetUserMyUserAddressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUserAddressAddress where
  parseJSON _ = undefined -- TODO: should be real function

newtype GetUserMyUserAliasAdressAddress = GetUserMyUserAliasAdressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUserAliasAdressAddress where
  parseJSON _ = undefined -- TODO: should be real function

data GetUserUserUser = GetUserUserUser
  { email :: String,
    name :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserUserUser where
  parseJSON _ = undefined -- TODO: should be real function

newtype GetUserArgs = GetUserArgs
  { coordinates :: Coordinates
  }
  deriving (Generic, Show, Eq)

instance ToJSON GetUserArgs where
  toJSON _ = undefined -- TODO: should be real function
