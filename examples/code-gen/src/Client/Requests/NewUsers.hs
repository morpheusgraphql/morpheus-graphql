{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# HLINT ignore "Use camelCase" #-}

module Client.Requests.NewUsers where

import Client.Schema
import Data.Morpheus.Client.Internal.CodeGen

instance RequestType NewUsers where
  type RequestArgs NewUsers = NewUsersArgs
  __name _ = "NewUsers"
  __query _ = "subscription NewUsers($loc: Coordinates!) {\n  newUser {\n    name\n    email\n    address(coordinates: $loc) {\n      city\n    }\n  }\n}\n"
  __type _ = Subscription

newtype NewUsers = NewUsers
  { newUser :: NewUsersNewUserUser
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewUsers where
  parseJSON = undefined -- TODO: should be real function

data NewUsersNewUserUser = NewUsersNewUserUser
  { name :: String,
    email :: String,
    address :: NewUsersNewUserAddressAddress
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewUsersNewUserUser where
  parseJSON = undefined -- TODO: should be real function

newtype NewUsersNewUserAddressAddress = NewUsersNewUserAddressAddress
  { city :: String
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewUsersNewUserAddressAddress where
  parseJSON = undefined -- TODO: should be real function

newtype NewUsersArgs = NewUsersArgs
  { loc :: Coordinates
  }
  deriving (Generic, Show, Eq)

instance ToJSON NewUsersArgs where
  toJSON {-- TODO: fix me --} = undefined -- TODO: should be real function
