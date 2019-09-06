{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Document.Declare
  ( declareTypes
  ) where

import           Data.Semigroup                           ((<>))
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Document.GQLType (deriveGQLType)
import           Data.Morpheus.Execution.Internal.Declare (declareResolverType, declareType)
import           Data.Morpheus.Types.Internal.DataD       (GQLTypeD (..), isInputKind)

declareTypes :: [GQLTypeD] -> Q [Dec]
declareTypes = fmap concat . traverse declareGQLType

declareGQLType :: GQLTypeD -> Q [Dec]
declareGQLType gqlType@GQLTypeD {typeD, typeKindD, typeArgD} = do
  let types = declareResolverType typeKindD derivingClasses typeD : map (declareType []) typeArgD
  typeClasses <- deriveGQLType gqlType
  pure $ types <> typeClasses
  where
    derivingClasses
      | isInputKind typeKindD = [''Show]
      | otherwise = []
