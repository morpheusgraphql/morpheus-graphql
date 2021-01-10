{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import Relude
import Test.Tasty
  ( defaultMain,
    testGroup,
  )
import Utils.Api
  ( apiTest,
  )
import qualified Utils.MergeSchema as MergeSchema

main :: IO ()
main = do
  mergeSchema <- MergeSchema.test
  defaultMain $
    testGroup
      "core tests"
      [ mergeSchema,
        apiTest "api/deity" ["simple", "interface"],
        apiTest
          "api/validation/fragment"
          [ "on-type",
            "on-interface",
            "on-interface-inline",
            "on-union-type",
            "fail-unknown-field-on-interface",
            "on-interface-type-casting",
            "on-interface-type-casting-inline",
            "on-interface-fail-without-casting"
          ]
      ]
