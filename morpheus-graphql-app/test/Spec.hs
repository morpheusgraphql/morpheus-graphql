{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import Relude
import Test.Morpheus.Utils
  ( deepScan,
    runCaseTree,
  )
import Test.Tasty
  ( defaultMain,
    testGroup,
  )
import Utils.Api
  ( runApiTest,
  )
import Utils.MergeSchema
  ( runMergeTest,
  )

main :: IO ()
main = do
  mergeTests <- runCaseTree runMergeTest <$> deepScan "merge"
  apiTests <- runCaseTree runApiTest <$> deepScan "api"
  defaultMain $
    testGroup
      "App Tests"
      [ mergeTests,
        apiTests
      ]
