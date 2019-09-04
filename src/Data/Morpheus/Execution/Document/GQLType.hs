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
import           Data.Typeable                      (Typeable)

deriveGQLType :: GQLTypeD -> Q [Dec]
deriveGQLType (TypeD {tName}, gqlKind, _) =
  pure <$> instanceD (cxt $ constrains gqlKind) (appT (conT ''GQLType) (genHeadSig gqlKind)) [methods]
  where
    genHeadSig KindObject = appT (conT $ mkName tName) (varT $ mkName "m")
    genHeadSig _          = conT $ mkName tName
    ----------
    constrains KindObject = [appT (conT ''Typeable) (varT $ mkName "m")]
    constrains _          = []
    ----
    methods = do
      typeN <- genHeadSig gqlKind
      pure $ TySynInstD ''KIND (TySynEqn [typeN] (ConT $ toKIND gqlKind))
    toKIND KindScalar      = ''SCALAR
    toKIND KindEnum        = ''ENUM
    toKIND KindObject      = ''OBJECT
    toKIND KindUnion       = ''UNION
    toKIND KindInputObject = ''INPUT_OBJECT
    toKIND KindList        = ''WRAPPER
    toKIND KindNonNull     = ''WRAPPER
    toKIND KindInputUnion  = ''INPUT_UNION
