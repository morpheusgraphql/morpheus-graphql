{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import qualified Case.Enum.Test as Enum
import qualified Case.Fragments.Test as Fragments
import qualified Case.Github.Test as Github
import qualified Case.Interface.Test as Interface
import qualified Case.LocalGlobal.Test as LocalGlobal
import qualified Case.LowercaseTypeName.Test as LowercaseTypeName
import qualified Case.ResponseTypes.Test as ResponseTypes
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
      "Client tests"
      [ Interface.test,
        LowercaseTypeName.test,
        LocalGlobal.test,
        Enum.test,
        Scalar.test,
        ResponseTypes.test,
        Github.test,
        Fragments.test
      ]
