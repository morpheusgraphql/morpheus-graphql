{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import Test.Tasty
  ( defaultMain,
    testGroup,
  )
import Utils.Api
  ( apiTest,
  )
import Utils.Schema
  ( testSchema,
  )
import Utils.SchemaMerging
  ( testSchemaMerging,
  )
import Prelude
  ( ($),
    IO,
  )

main :: IO ()
main = do
  schemaMerging <- testSchemaMerging
  schema <- testSchema
  defaultMain $
    testGroup
      "core tests"
      [ schema,
        schemaMerging,
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
