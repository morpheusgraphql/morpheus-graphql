{-# LANGUAGE OverloadedStrings #-}

module Subscription.Test (testSubsriptions) where

import qualified Subscription.API as TS
import Subscription.Case.ApolloRequest (testApolloRequest)
import Subscription.Case.Publishing (testPublishing)
import Test.Tasty (TestTree, testGroup)

testSubsriptions :: IO TestTree
testSubsriptions = do
  subscription <- testApolloRequest TS.api
  publishing <- testPublishing
  return $ testGroup "Subscription" [subscription, publishing]
