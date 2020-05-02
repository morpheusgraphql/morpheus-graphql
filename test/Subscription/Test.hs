{-# LANGUAGE OverloadedStrings #-}

module Subscription.Test (testSubsriptions) where

import qualified Subscription.API as TS
import Subscription.Case.ApolloRequest (testApolloRequest)
import Test.Tasty (TestTree, testGroup)

testSubsriptions :: IO TestTree
testSubsriptions = do
  subscription <- testApolloRequest TS.api
  return $ testGroup "Subscription" [subscription]
