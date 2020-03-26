{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import qualified Feature.Holistic.API          as Holistic
                                                ( api )
import qualified Feature.Input.Enum.API        as InputEnum
                                                ( api )
import qualified Feature.Input.Scalar.API      as InputScalar
                                                ( api )
import qualified Feature.Input.Object.API      as InputObject
                                                ( api )
import qualified Feature.InputType.API         as InputType
                                                ( api )
import qualified Feature.Schema.API            as Schema
                                                ( api )
import qualified Feature.UnionType.API         as UnionType
                                                ( api )
import qualified Feature.WrappedTypeName.API   as TypeName
                                                ( api )
import qualified Feature.TypeInference.API     as Inference
                                                ( api )
import           Rendering.TestSchemaRendering  ( testSchemaRendering )
import           Test.Tasty                     ( defaultMain
                                                , testGroup
                                                )
import           TestFeature                    ( testFeature )

main :: IO ()
main = do
  ioTests     <- testFeature Holistic.api "Feature/Holistic"
  unionTest   <- testFeature UnionType.api "Feature/UnionType"
  inputTest   <- testFeature InputType.api "Feature/InputType"
  schemaTest  <- testFeature Schema.api "Feature/Schema"
  typeName    <- testFeature TypeName.api "Feature/WrappedTypeName"
  inputEnum   <- testFeature InputEnum.api "Feature/Input/Enum"
  inputScalar <- testFeature InputScalar.api "Feature/Input/Scalar"
  inputObject <- testFeature InputObject.api "Feature/Input/Object"
  inference   <- testFeature Inference.api "Feature/TypeInference"
  defaultMain
    (testGroup
      "Morpheus Graphql Tests"
      [ ioTests
      , unionTest
      , inputTest
      , schemaTest
      , typeName
      , inputEnum
      , inputScalar
      , inputObject
      , testSchemaRendering
      , inference
      ]
    )
