{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import Data.Morpheus (runApp)
import Data.Morpheus.Types (GQLRequest (..), GQLResponse (..))
import qualified Feature.Holistic.API as Holistic
import qualified Feature.Input.DefaultValues as DefaultValues
import qualified Feature.NamedResolvers.API as NamedResolvers
import Relude
import Rendering.TestSchemaRendering (testSchemaRendering)
import Subscription.Test (testSubscriptions)
import Test.Morpheus
  ( FileUrl,
    cd,
    mainTest,
    mkUrl,
    scan,
    testApi,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )

mkFeatureUrl :: FilePath -> FilePath -> FileUrl
mkFeatureUrl groupName = cd (cd (mkUrl "Feature") groupName)

testFeature :: FilePath -> (GQLRequest -> IO GQLResponse, FilePath) -> IO TestTree
testFeature groupName (api, name) = scan (testApi api) (mkFeatureUrl groupName name)

testFeatures :: FilePath -> [(GQLRequest -> IO GQLResponse, FilePath)] -> IO TestTree
testFeatures name cases =
  testGroup name
    <$> traverse
      (testFeature name)
      cases

main :: IO ()
main =
  mainTest
    "Morpheus Graphql Tests"
    [ testFeatures
        "Input"
        [ (Variables.api, "variables"),
          (Enums.api, "enums"),
          (Scalars.api, "scalars"),
          (Objects.api, "objects"),
          (DefaultValues.api, "default-values"),
          (Collections.api, "collections")
        ],
      testFeatures
        "Collision"
        [ (TypeCategoryCollisionSuccess.api, "category-collision-success"),
          (TypeCategoryCollisionFail.api, "category-collision-fail"),
          (NameCollision.api, "name-collision")
        ],
      testFeatures
        "Inference"
        [ (WrappedType.api, "wrapped-type"),
          (TypeGuards.api, "type-guards"),
          (UnionType.api, "union-type"),
          (TypeInference.api, "type-inference"),
          (TaggedArguments.api, "tagged-arguments"),
          (TaggedArgumentsFail.api, "tagged-arguments-fail"),
          (ObjectAndEnum.api, "object-and-enum")
        ],
      testFeatures
        "Directive"
        [ (DirectiveDefinition.api, "definition")
        ],
      testFeatures
        "Holistic"
        [ (Holistic.api, "holistic")
        ],
      testFeatures
        "NamedResolvers"
        [(runApp NamedResolvers.app, "tests")],
      testSubscriptions,
      pure testSchemaRendering
    ]
