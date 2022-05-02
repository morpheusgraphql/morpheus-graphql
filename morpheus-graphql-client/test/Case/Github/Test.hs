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
    raw,
  )
import Relude
import Spec.Utils
  ( assertFetch,
    defineClientWith,
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

defineClientWith
  "Github"
  [raw|
    query GetTags ($user: String!, $repo: String!)
      {
        repository(owner: $user, name: $repo) {
          refs(refPrefix: "refs/tags/", first: 100) {
            pageInfo {
              endCursor
              hasNextPage
            }
            edges {
              cursor
              node {
                name
                target {
                  __typename
                  ... on Tag {
                    tagger {
                      date
                    }
                  }
                }
              }
            }
          }
        }
      }
  |]

test :: TestTree
test =
  assertFetch
    "Github"
    Nothing
    GetTagsArgs
      { user = "UserName",
        repo = "repoName"
      }
    GetTags
      { repository =
          Just
            RepositoryRepository
              { refs =
                  Just
                    RepositoryRefsRefConnection
                      { pageInfo =
                          RepositoryRefsPageInfoPageInfo
                            { endCursor = Just "test value 1",
                              hasNextPage = False
                            },
                        edges =
                          Just
                            [ Just
                                RepositoryRefsEdgesRefEdge
                                  { cursor = "test cursor",
                                    node =
                                      Just
                                        RepositoryRefsEdgesNodeRef
                                          { name = "test name",
                                            target =
                                              Just
                                                RepositoryRefsEdgesNodeTargetGitObject
                                                  { __typename = "GitObject"
                                                  }
                                          }
                                  }
                            ]
                      }
              }
      }
