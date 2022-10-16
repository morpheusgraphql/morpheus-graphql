{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# HLINT ignore "Use camelCase" #-}

module Client.Requests.GetUsers where

import Client.Schema
import GHC.Generics (Generic)

instance RequestType GetUser where
  type RequestArgs GetUser = GetUserArgs
  __name _ = undefined
  __query _ = undefined
  __type _ = undefined

data GetUser = GetUser
  { myUser :: GetUserMyUserUser,
    user :: GetUserUserUser
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUser where
  parseJSON _ = undefined

data GetUserMyUserUser = GetUserMyUserUser
  { name :: String,
    aliasEmail :: String,
    address :: GetUserMyUserAddressAddress,
    aliasAdress :: GetUserMyUserAliasAdressAddress
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUserUser where
  parseJSON _ = undefined

newtype GetUserMyUserAddressAddress = GetUserMyUserAddressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUserAddressAddress where
  parseJSON _ = undefined

newtype GetUserMyUserAliasAdressAddress = GetUserMyUserAliasAdressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserMyUserAliasAdressAddress where
  parseJSON _ = undefined

data GetUserUserUser = GetUserUserUser
  { email :: String,
    name :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUserUserUser where
  parseJSON _ = undefined

newtype GetUserArgs = GetUserArgs
  { coordinates :: Coordinates
  }
  deriving (Generic, Show, Eq)

instance ToJSON GetUserArgs where
  toJSON _ = undefined
