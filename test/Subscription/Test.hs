{-# LANGUAGE OverloadedStrings #-}

module Subscription.Test (testSubscriptions) where

import qualified Subscription.API as TS
import Subscription.Case.ApolloRequest (testApolloRequest)
import Subscription.Case.Publishing (testPublishing)
import Test.Tasty (TestTree, testGroup)

testSubscriptions :: IO TestTree
testSubscriptions = do
  subscription <- testApolloRequest TS.app
  publishing <- testPublishing
  return $ testGroup "Subscription" [subscription, publishing]
