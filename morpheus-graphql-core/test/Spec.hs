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
        apiTest "api/deity" ["simple", "interface"]
      ]
