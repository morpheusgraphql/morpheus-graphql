{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# HLINT ignore "Use camelCase" #-}

module Client.Requests.GetUsers where

import Client.Schema
import GHC.Generics (Generic)

-- TODO: "GetUser" RequestTypeClass

data GetUser = GetUser
  { myUser :: GetUserMyUserUser,
    user :: GetUserUserUser
  }
  deriving (Generic, Show, Eq)

-- TODO: "GetUser" FromJSONClass

data GetUserMyUserUser = GetUserMyUserUser
  { name :: String,
    aliasEmail :: String,
    address :: GetUserMyUserAddressAddress,
    aliasAdress :: GetUserMyUserAliasAdressAddress
  }
  deriving (Generic, Show, Eq)

-- TODO: "GetUserMyUserUser" FromJSONClass

newtype GetUserMyUserAddressAddress = GetUserMyUserAddressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

-- TODO: "GetUserMyUserAddressAddress" FromJSONClass

newtype GetUserMyUserAliasAdressAddress = GetUserMyUserAliasAdressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

-- TODO: "GetUserMyUserAliasAdressAddress" FromJSONClass

data GetUserUserUser = GetUserUserUser
  { email :: String,
    name :: String
  }
  deriving (Generic, Show, Eq)

-- TODO: "GetUserUserUser" FromJSONClass

newtype GetUserArgs = GetUserArgs
  { coordinates :: Coordinates
  }
  deriving (Generic, Show, Eq)

-- TODO: "GetUserArgs" ToJSONClass
