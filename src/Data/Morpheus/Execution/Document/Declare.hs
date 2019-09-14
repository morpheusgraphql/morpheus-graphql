{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Document.Declare
  ( declareTypes
  ) where

import           Control.Lens                                (declareLenses)
import           Data.Semigroup                              ((<>))
import           Language.Haskell.TH

import           Data.Morpheus.Execution.Document.Decode     (deriveDecode)

--
-- MORPHEUS
import           Data.Morpheus.Execution.Document.GQLType    (deriveGQLType)
import           Data.Morpheus.Execution.Document.Introspect (deriveArguments, deriveIntrospect)
import           Data.Morpheus.Execution.Internal.Declare    (declareResolverType, declareType)
import           Data.Morpheus.Types.Internal.DataD          (GQLTypeD (..), isInputKind)

declareTypes :: [GQLTypeD] -> Q [Dec]
declareTypes = fmap concat . traverse declareGQLType

declareGQLType :: GQLTypeD -> Q [Dec]
declareGQLType gqlType@GQLTypeD {typeD, typeKindD, typeArgD} = do
  types <- declareGQL
  introspectArgs <- concat <$> traverse deriveArguments typeArgD
  introspection <- deriveGQLInstances
  typeClasses <- deriveGQLType gqlType
  pure $ types <> typeClasses <> map (declareType []) typeArgD <> introspection  <> introspectArgs
  where
    deriveGQLInstances
      | isInputKind typeKindD = concat <$> traverse (\x -> x typeD) [deriveIntrospect, deriveDecode]
      | otherwise = pure []
    declareGQL
      | isInputKind typeKindD = declareLenses declareT
      | otherwise = declareT
      where
        declareT = pure [declareResolverType typeKindD derivingClasses typeD]
    derivingClasses
      | isInputKind typeKindD = [''Show]
      | otherwise = []
