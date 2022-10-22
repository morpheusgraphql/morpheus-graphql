{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Client.Queries.GetUsers where

import Client.Blog
import Data.Morpheus.Client.CodeGen.Internal
import Scalars

instance RequestType GetUsers where
  type RequestArgs GetUsers = ()
  __name _ = "GetUsers"
  __query _ = "query GetUsers {\n  users {\n    name\n    posts {\n      title\n      body\n    }\n  }\n}\n"
  __type _ = Query

newtype GetUsers = GetUsers
  { users :: [GetUsersUsers]
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUsers where
  parseJSON =
    withObject "GetUsers" (\v -> GetUsers <$> v .: "users")

data GetUsersUsers = GetUsersUsers
  { name :: String,
    posts :: [GetUsersUsersPosts]
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUsersUsers where
  parseJSON =
    withObject "GetUsersUsers" (\v -> GetUsersUsers <$> v .: "name" <*> v .: "posts")

data GetUsersUsersPosts = GetUsersUsersPosts
  { title :: String,
    body :: Maybe Markdown
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetUsersUsersPosts where
  parseJSON =
    withObject "GetUsersUsersPosts" (\v -> GetUsersUsersPosts <$> v .: "title" <*> v .:? "body")
