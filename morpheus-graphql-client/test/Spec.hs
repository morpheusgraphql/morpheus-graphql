{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import qualified Case.Enum.Test as Enum
import Case.Interface.Test (testInterface)
import qualified Case.JSON.Custom.Errors as JSONCustomErrors
import qualified Case.JSON.Custom.ErrorsWithType as JSONCustomErrorsWithType
import qualified Case.JSON.Custom.Mutation as JSONCustomMutation
import qualified Case.JSON.Custom.NoResponseOrError as JSONNoResponseOrError
import qualified Case.JSON.Custom.PartialResponse as JSONCustomPartialResponse
import qualified Case.JSON.Custom.Query as JSONCustomQuery
import qualified Case.JSON.Custom.Subscription as JSONCustomSubscription
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
        JSONCustomErrors.test,
        JSONCustomErrorsWithType.test,
        JSONCustomMutation.test,
        JSONCustomPartialResponse.test,
        JSONCustomQuery.test,
        JSONCustomSubscription.test,
        JSONNoResponseOrError.test
      ]
