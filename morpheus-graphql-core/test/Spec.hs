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
import qualified Utils.Rendering as Rendering
import Utils.Schema
  ( testSchema,
  )

main :: IO ()
main = do
  schema <- testSchema
  defaultMain $
    testGroup
      "core tests"
      [ schema,
        Rendering.test
          "rendering/simple"
          [ "simple",
            "nested",
            "query",
            "mutation",
            "subscription",
            "directive"
          ],
        Rendering.test
          "rendering/union"
          [ "interface",
            "union"
          ],
        Rendering.test
          "rendering/variable"
          [ "simple",
            "input",
            "enum",
            "list",
            "include-exclude"
          ]
      ]
