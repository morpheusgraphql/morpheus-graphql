{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Feature.Holistic.API  as Holistic (api)
import qualified Feature.UnionType.API as UnionType (api)
import           Test.Tasty            (defaultMain, testGroup)
import           TestFeature           (testFeature)

main :: IO ()
main = do
  ioTests <- testFeature Holistic.api "Feature/Holistic"
  unionTest <- testFeature UnionType.api "Feature/UnionType"
  defaultMain (testGroup "Morpheus Graphql Tests" [ioTests, unionTest])
