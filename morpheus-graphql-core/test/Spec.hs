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
import Prelude
  ( ($),
    IO,
  )

main :: IO ()
main = do
  schema <- testSchema
  defaultMain $
    testGroup
      "core tests"
      [ schema,
        apiTest "api/deity" ["simple", "interface"],
        apiTest
          "api/validation/fragment"
          [ "on-type",
            "on-interface",
            "inline-on-interface",
            "on-union-type",
            "fail-unknown-field-on-interface",
            "on-interface-type-casting"
          ]
      ]
