{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Client.Queries.GetUsers where

import Data.Morpheus.Client.CodeGen.Internal
import Scalars (Markdown)

instance RequestType GetUsers where
  type RequestArgs GetUsers = ()
  __name _ = "GetUsers"
  __query _ = "query GetUsers {\n  getUsers {\n    name\n    posts {\n      title\n      body\n    }\n  }\n}\n"
  __type _ = OPERATION_QUERY

newtype GetUsers = GetUsers
  { getUsers :: [GetUsersGetUsers]
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUsers where
  parseJSON =
    withObject "GetUsers" (\v -> GetUsers <$> v .: "getUsers")

data GetUsersGetUsers = GetUsersGetUsers
  { name :: String,
    posts :: [GetUsersGetUsersPosts]
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUsersGetUsers where
  parseJSON =
    withObject "GetUsersGetUsers" (\v -> GetUsersGetUsers <$> v .: "name" <*> v .: "posts")

data GetUsersGetUsersPosts = GetUsersGetUsersPosts
  { title :: String,
    body :: Maybe Markdown
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUsersGetUsersPosts where
  parseJSON =
    withObject "GetUsersGetUsersPosts" (\v -> GetUsersGetUsersPosts <$> v .: "title" <*> v .:? "body")
