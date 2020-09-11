{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import Case.Interface.Test (testInterface)
import Case.LowercaseTypeName.Test
  ( testLowercaseTypeName,
  )
import Test.Tasty
  ( defaultMain,
    testGroup,
  )
import Prelude
  ( ($),
    IO,
  )

main :: IO ()
main =
  defaultMain $
    testGroup
      "client tests"
      [ testInterface,
        testLowercaseTypeName
      ]
