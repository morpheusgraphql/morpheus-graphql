{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.Github.Test
  ( test,
  )
where

import Data.Morpheus.Client
  ( DecodeScalar (..),
    EncodeScalar (..),
    ScalarValue (..),
    declareGlobalTypesByName,
    declareLocalTypes,
  )
import Relude
import Spec.Utils
  ( assertFetch,
    path,
  )
import Test.Tasty
  ( TestTree,
  )

newtype GitTimestamp = GitTimestamp
  { unGitTimestamp :: Text
  }
  deriving (Eq, Show)

instance DecodeScalar GitTimestamp where
  decodeScalar (String x) = pure (GitTimestamp x)
  decodeScalar _ = Left ""

instance EncodeScalar GitTimestamp where
  encodeScalar (GitTimestamp x) = String x

declareGlobalTypesByName
  (path "Github/schema.gql")
  ["GitTimestamp"]

declareLocalTypes
  (path "Github/schema.gql")
  (path "Github/query.gql")

test :: TestTree
test =
  assertFetch
    "Github"
    Nothing
    GetTagsArgs
      { user = "UserName",
        repo = "repoName"
      }
    ( Right
        GetTags
          { repository =
              Just
                GetTagsRepositoryRepository
                  { refs =
                      Just
                        GetTagsRepositoryRefsRefConnection
                          { pageInfo =
                              GetTagsRepositoryRefsPageInfoPageInfo
                                { endCursor = Just "test value 1",
                                  hasNextPage = False
                                },
                            edges =
                              Just
                                [ Just
                                    GetTagsRepositoryRefsEdgesRefEdge
                                      { cursor = "test cursor",
                                        node =
                                          Just
                                            GetTagsRepositoryRefsEdgesNodeRef
                                              { name = "test name",
                                                target =
                                                  Just
                                                    GetTagsRepositoryRefsEdgesNodeTargetGitObject
                                                      { __typename = "GitObject"
                                                      }
                                              }
                                      }
                                ]
                          }
                  }
          }
    )
