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
import           Data.Morpheus.Types.Internal.DataD (GQLTypeD, TypeD (..))

deriveGQLType :: GQLTypeD -> Q [Dec]
deriveGQLType (TypeD {tName}, gqlKind, _) =
  pure <$> instanceD (cxt []) (appT (conT ''GQLType) (conT $ mkName tName)) methods
  where
    methods = [pure $ TySynInstD ''KIND (TySynEqn [ConT $ mkName tName] (ConT $ toKIND gqlKind))]
    toKIND KindScalar      = ''SCALAR
    toKIND KindEnum        = ''ENUM
    toKIND KindObject      = ''OBJECT
    toKIND KindUnion       = ''UNION
    toKIND KindInputObject = ''INPUT_OBJECT
    toKIND KindList        = ''WRAPPER
    toKIND KindNonNull     = ''WRAPPER
    toKIND KindInputUnion  = ''INPUT_UNION
