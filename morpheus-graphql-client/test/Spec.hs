{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import Case.Interface.Test (testInterface)
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
      [testInterface]
