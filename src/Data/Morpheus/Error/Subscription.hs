{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Subscription
  ( subscriptionIsNotDefined
  )
where

import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Position )
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( GQLErrors )

subscriptionIsNotDefined :: Position -> GQLErrors
subscriptionIsNotDefined position' =
  errorMessage position' "Schema is not configured for subscriptions."
