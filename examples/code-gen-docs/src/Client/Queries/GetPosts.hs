{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Client.Queries.GetPosts where

import Client.Blog
import Data.Morpheus.Client.CodeGen.Internal
import Scalars

instance RequestType GetPosts where
  type RequestArgs GetPosts = ()
  __name _ = "GetPosts"
  __query _ = "query GetPosts {\n  getPosts {\n    title\n    body\n  }\n}\n"
  __type _ = Query

newtype GetPosts = GetPosts
  { getPosts :: [GetPostsGetPosts]
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetPosts where
  parseJSON =
    withObject "GetPosts" (\v -> GetPosts <$> v .: "getPosts")

data GetPostsGetPosts = GetPostsGetPosts
  { title :: String,
    body :: Maybe Markdown
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetPostsGetPosts where
  parseJSON =
    withObject "GetPostsGetPosts" (\v -> GetPostsGetPosts <$> v .: "title" <*> v .:? "body")
