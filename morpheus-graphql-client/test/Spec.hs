{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import qualified Case.Enum.Test as Enum
import qualified Case.Github.Test as GH
import Case.Interface.Test (testInterface)
import qualified Case.JSON.Custom.Test as JSONCustom
import qualified Case.LocalGlobal.Test as LG
import Case.LowercaseTypeName.Test
  ( testLowercaseTypeName,
  )
import qualified Case.Scalar.Test as Scalar
import Test.Tasty
  ( defaultMain,
    testGroup,
  )
import Prelude
  ( IO,
    ($),
  )

main :: IO ()
main =
  defaultMain $
    testGroup
      "client tests"
      [ testInterface,
        testLowercaseTypeName,
        LG.test,
        Enum.test,
        Scalar.test,
        JSONCustom.test,
        GH.test
      ]
