{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeFamilies #-}

{-# HLINT ignore "Use camelCase" #-}

module Client.Requests.NewUsers where

import Client.Schema
import Data.Morpheus.Client.Internal.CodeGen

instance RequestType NewUsers where
  type RequestArgs NewUsers = NewUsersArgs
  __name _ = undefined -- TODO: should be real function
  __query _ = undefined -- TODO: should be real function
  __type _ = undefined -- TODO: should be real function

newtype NewUsers = NewUsers
  { newUser :: NewUsersNewUserUser
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewUsers where
  parseJSON _ = undefined -- TODO: should be real function

data NewUsersNewUserUser = NewUsersNewUserUser
  { name :: String,
    email :: String,
    address :: NewUsersNewUserAddressAddress
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewUsersNewUserUser where
  parseJSON _ = undefined -- TODO: should be real function

newtype NewUsersNewUserAddressAddress = NewUsersNewUserAddressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewUsersNewUserAddressAddress where
  parseJSON _ = undefined -- TODO: should be real function

newtype NewUsersArgs = NewUsersArgs
  { loc :: Coordinates
  }
  deriving (Generic, Show, Eq)

instance ToJSON NewUsersArgs where
  toJSON _ = undefined -- TODO: should be real function
