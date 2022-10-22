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
  __query _ = "query GetPosts {\n  posts {\n    title\n    body\n  }\n}\n"
  __type _ = Query

newtype GetPosts = GetPosts
  { posts :: [GetPostsPosts]
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetPosts where
  parseJSON =
    withObject "GetPosts" (\v -> GetPosts <$> v .: "posts")

data GetPostsPosts = GetPostsPosts
  { title :: String,
    body :: Maybe Markdown
  }
  deriving (Generic, Show, Eq)

instance FromJSON GetPostsPosts where
  parseJSON =
    withObject "GetPostsPosts" (\v -> GetPostsPosts <$> v .: "title" <*> v .:? "body")
