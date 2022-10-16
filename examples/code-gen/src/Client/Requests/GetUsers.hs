{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# HLINT ignore "Use camelCase" #-}

module Client.Requests.GetUsers where

import Client.Schema
import Data.Text (Text)
import GHC.Generics (Generic)
import Globals.GQLScalars

-- TODO: RequestTypeClass
data GetUser = GetUser
  { myUser :: GetUserMyUserUser,
    user :: GetUserUserUser
  }
  deriving (Generic, Show, Eq)

-- TODO: FromJSONClass
data GetUserMyUserUser = GetUserMyUserUser
  { name :: String,
    aliasEmail :: String,
    address :: GetUserMyUserAddressAddress,
    aliasAdress :: GetUserMyUserAliasAdressAddress
  }
  deriving (Generic, Show, Eq)

-- TODO: FromJSONClass
newtype GetUserMyUserAddressAddress = GetUserMyUserAddressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

-- TODO: FromJSONClass
newtype GetUserMyUserAliasAdressAddress = GetUserMyUserAliasAdressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

-- TODO: FromJSONClass
data GetUserUserUser = GetUserUserUser
  { email :: String,
    name :: String
  }
  deriving (Generic, Show, Eq)

-- TODO: FromJSONClass
newtype GetUserArgs = GetUserArgs
  { coordinates :: Coordinates
  }
  deriving (Generic, Show, Eq)

-- TODO: ToJSONClass
