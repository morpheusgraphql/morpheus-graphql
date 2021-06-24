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
import Utils.Rendering
  ( runRenderingTest,
  )
import Utils.Schema
  ( runSchemaTest,
  )

main :: IO ()
main = do
  schema <- runCaseTree runSchemaTest <$> deepScan "schema"
  renderings <- runCaseTree runRenderingTest <$> deepScan "rendering"
  defaultMain $
    testGroup
      "core tests"
      [ schema,
        renderings
      ]
