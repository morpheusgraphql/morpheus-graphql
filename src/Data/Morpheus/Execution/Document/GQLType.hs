{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.GQLType
  ( deriveGQLType
  ) where

import           Language.Haskell.TH

import           Data.Morpheus.Kind                 (ENUM, INPUT_OBJECT, INPUT_UNION, OBJECT, SCALAR, UNION, WRAPPER)

--
-- MORPHEUS
import           Data.Morpheus.Types.GQLType        (GQLType (..))
import           Data.Morpheus.Types.Internal.Data  (DataTypeKind (..))
import           Data.Morpheus.Types.Internal.DataD (GQLTypeD (..), TypeD (..), unKindD)
import           Data.Typeable                      (Typeable)

deriveGQLType :: GQLTypeD -> Q [Dec]
deriveGQLType GQLTypeD {typeD = TypeD {tName}, typeKindD} =
  pure <$> instanceD (cxt constrains) (appT (conT ''GQLType) genHeadSig) [methods]
  where
    gqlKind = unKindD typeKindD
    withVar = gqlKind == KindObject || gqlKind == KindUnion
    genHeadSig
      | withVar = appT (conT $ mkName tName) (varT $ mkName "m")
      | otherwise = conT $ mkName tName
    ----------
    constrains
      | withVar = [appT (conT ''Typeable) (varT $ mkName "m")]
      | otherwise = []
    ----
    methods = do
      typeN <- genHeadSig
      pure $ TySynInstD ''KIND (TySynEqn [typeN] (ConT $ toKIND gqlKind))
    toKIND KindScalar      = ''SCALAR
    toKIND KindEnum        = ''ENUM
    toKIND KindObject      = ''OBJECT
    toKIND KindUnion       = ''UNION
    toKIND KindInputObject = ''INPUT_OBJECT
    toKIND KindList        = ''WRAPPER
    toKIND KindNonNull     = ''WRAPPER
    toKIND KindInputUnion  = ''INPUT_UNION
