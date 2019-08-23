{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Execution.Document.Declare
  ( declareTypes
  ) where

import           Language.Haskell.TH

import           Data.Morpheus.Execution.Document.GQLType (deriveGQLType)

--
-- MORPHEUS
import           Data.Morpheus.Execution.Internal.Declare (declareType)
import           Data.Morpheus.Types.Internal.DataD       (GQLTypeD)

declareTypes :: [GQLTypeD] -> Q [Dec]
declareTypes = fmap concat . traverse declareGQLType

declareGQLType :: GQLTypeD -> Q [Dec]
declareGQLType gqlType@(typeD, _, _) = do
  let decT = declareType [] typeD
  gqlTypeDec <- deriveGQLType gqlType
  pure $ decT : gqlTypeDec
