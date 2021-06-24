{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import Data.Aeson (toJSON)
import Data.Morpheus.Types (GQLRequest (..), GQLResponse (..))
import qualified Feature.Holistic.API as Holistic
  ( api,
  )
import qualified Feature.Input.DefaultValues as DefaultValues
  ( api,
  )
import qualified Feature.Input.Enums as Enums
  ( api,
  )
import qualified Feature.Input.Objects as Objects
  ( api,
  )
import qualified Feature.Input.Scalars as Scalars
  ( api,
  )
import qualified Feature.Input.Variables as Variables
  ( api,
  )
import qualified Feature.Schema.API as Schema
  ( api,
  )
import qualified Feature.TypeCategoryCollision.Fail as TypeCategoryCollisionFail
import qualified Feature.TypeCategoryCollision.Success as TypeCategoryCollisionSuccess
import qualified Feature.TypeGuards.API as TypeGuards
  ( api,
  )
import qualified Feature.TypeInference.API as Inference
  ( api,
  )
import qualified Feature.UnionType.API as UnionType
  ( api,
  )
import qualified Feature.WrappedTypeName.API as TypeName
  ( api,
  )
import Relude
import Rendering.TestSchemaRendering (testSchemaRendering)
import Subscription.Test (testSubsriptions)
import Test.Morpheus.Utils
  ( FileUrl (..),
    assertEqualResponse,
    cd,
    deepScan,
    getRequestBy,
    getResponse,
    recursiveScan,
    runCaseTree,
    scanDirectories,
  )
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )
import Test.Tasty.HUnit (testCase)

testByFiles2 :: (GQLRequest -> IO GQLResponse) -> FileUrl -> [FileUrl] -> [TestTree]
testByFiles2 testApi url _ =
  [ testCase (fileName url) $ do
      expectedResponse <- getResponse url
      actualResponse <- getRequestBy (GQLRequest Nothing) url >>= testApi
      assertEqualResponse expectedResponse (toJSON actualResponse)
  ]

testFeature2 :: (GQLRequest -> IO GQLResponse) -> FilePath -> IO TestTree
testFeature2 api name = do
  tests <- deepScan ("Feature/" <> name <> "/tests")
  pure $ runCaseTree (testByFiles2 api) tests

testFeature :: FileUrl -> (GQLRequest -> IO GQLResponse) -> FilePath -> IO TestTree
testFeature url api name = do
  tests <- recursiveScan scanDirectories (cd url name)
  pure $ runCaseTree (testByFiles2 api) tests

testInputs :: IO TestTree
testInputs =
  testGroup "input"
    <$> traverse
      (uncurry (testFeature (FileUrl ["Feature", "test"] "Input")))
      [ (Variables.api, "variables"),
        (Enums.api, "enums"),
        (Scalars.api, "scalars"),
        (Objects.api, "objects"),
        (DefaultValues.api, "default-values")
      ]

testTypeCategory :: IO TestTree
testTypeCategory =
  testGroup "TypeCategoryCollision"
    <$> traverse
      (uncurry (testFeature (FileUrl ["Feature", "test"] "TypeCategoryCollision")))
      [ (TypeCategoryCollisionSuccess.api, "success"),
        (TypeCategoryCollisionFail.api, "fail")
      ]

main :: IO ()
main = do
  tests <-
    traverse
      (uncurry testFeature2)
      [ (UnionType.api, "UnionType"),
        (Schema.api, "Schema"),
        (Inference.api, "TypeInference"),
        (TypeGuards.api, "TypeGuards"),
        (TypeName.api, "WrappedTypeName"),
        (Holistic.api, "Holistic")
      ]
  rest <-
    sequence
      [ testInputs,
        testTypeCategory,
        testSubsriptions
      ]
  defaultMain
    ( testGroup
        "Morpheus Graphql Tests"
        ( testSchemaRendering : rest
            <> tests
        )
    )
