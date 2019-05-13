{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Mutation
  ( mutationIsNotDefined
  ) where

import           Data.Morpheus.Error.Utils    (errorMessage)
import           Data.Morpheus.Types.Error    (GQLErrors)
import           Data.Morpheus.Types.MetaInfo (Position)

mutationIsNotDefined :: Position -> GQLErrors
mutationIsNotDefined position' = errorMessage position' "Schema is not configured for mutations."
