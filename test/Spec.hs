{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

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
    cd,
    deepScan,
    foldCaseTree,
    recursiveScan,
    scanDirectories,
    testApi,
  )
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
  )

mkUrl :: FilePath -> FileUrl
mkUrl fileName =
  FileUrl
    { filePath = ["Feature", "test"],
      fileName,
      isDir = True
    }

testFeature2 :: (GQLRequest -> IO GQLResponse) -> FilePath -> IO TestTree
testFeature2 api name =
  foldCaseTree (testApi api)
    <$> deepScan ("Feature/" <> name <> "/tests")

testFeature :: FilePath -> (GQLRequest -> IO GQLResponse, FilePath) -> IO TestTree
testFeature groupName (api, name) =
  foldCaseTree (testApi api)
    <$> recursiveScan scanDirectories (cd (mkUrl groupName) name)

testFeatures :: FilePath -> [(GQLRequest -> IO GQLResponse, FilePath)] -> IO TestTree
testFeatures name cases =
  testGroup name
    <$> traverse
      (testFeature name)
      cases

testInputs :: IO TestTree
testInputs =
  testFeatures
    "Input"
    [ (Variables.api, "variables"),
      (Enums.api, "enums"),
      (Scalars.api, "scalars"),
      (Objects.api, "objects"),
      (DefaultValues.api, "default-values")
    ]

testTypeCategory :: IO TestTree
testTypeCategory =
  testFeatures
    "TypeCategoryCollision"
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
