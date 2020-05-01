{-# LANGUAGE OverloadedStrings #-}

module Subscription.Test (testSubsriptions) where

import qualified Subscription.API as TS
import Subscription.TestSubscription (testSubscription)
import Test.Tasty (TestTree, testGroup)

testSubsriptions :: IO TestTree
testSubsriptions = do
  subscription <- testSubscription TS.api "Subscription"
  return $ testGroup "Subscription" [subscription]
