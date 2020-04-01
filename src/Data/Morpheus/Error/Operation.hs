{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Mutation
  ( mutationIsNotDefined
  )
where

import           Data.Morpheus.Error.Utils      ( errorMessage )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Position 
                                                , GQLErrors 
                                                )


mutationIsNotDefined :: Position -> GQLErrors
mutationIsNotDefined position' =
  errorMessage position' "Schema is not configured for mutations."

subscriptionIsNotDefined :: Position -> GQLErrors
subscriptionIsNotDefined position' =
  errorMessage position' "Schema is not configured for subscriptions."
