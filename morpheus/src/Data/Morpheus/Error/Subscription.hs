{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Subscription
  ( subscriptionIsNotDefined
  ) where

import           Data.Morpheus.Error.Utils    (errorMessage)
import           Data.Morpheus.Types.Error    (GQLErrors)
import           Data.Morpheus.Types.MetaInfo (Position)

subscriptionIsNotDefined :: Position -> GQLErrors
subscriptionIsNotDefined position' = errorMessage position' "Schema is not configured for subscriptions."
