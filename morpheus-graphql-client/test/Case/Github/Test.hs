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
  ( testInterface,
  )
where

import Data.Morpheus.Client
  ( DecodeScalar (..),
    EncodeScalar (..),
    Fetch (..),
    FetchError,
    ScalarValue (..),
    raw,
  )
import Relude
import Spec.Utils
  ( defineClientWith,
    mockApi,
  )
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
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

client :: IO (Either (FetchError GetTags) GetTags)
client =
  fetch
    (mockApi "Interface")
    GetTagsArgs
      { user = "UserName",
        repo = "repoName"
      }

testInterface :: TestTree
testInterface = testCase "test Github interfaces" $ do
  value <- client
  assertEqual
    "test Github interface"
    ( Right
        ( GetTags
            { repository =
                Just
                  RepositoryRepository
                    { refs =
                        Just
                          RepositoryRefsRefConnection
                            { pageInfo =
                                RepositoryRefsPageInfoPageInfo
                                  { endCursor = Just "",
                                    hasNextPage = False
                                  },
                              edges =
                                Just
                                  [ Just
                                      RepositoryRefsEdgesRefEdge
                                        { cursor = "",
                                          node =
                                            Just
                                              RepositoryRefsEdgesNodeRef
                                                { name = "",
                                                  target =
                                                    Just
                                                      RepositoryRefsEdgesNodeTargetGitObject
                                                        { __typename = ""
                                                        }
                                                }
                                        }
                                  ]
                            }
                    }
            }
        )
    )
    value
