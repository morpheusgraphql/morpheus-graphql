{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.TypeName where

import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
  )
import Data.Morpheus.Server.Types.SchemaT
  ( TypeFingerprint (..),
  )
import Data.Morpheus.Server.Types.Types
  ( Pair,
  )
import Data.Morpheus.Types.Internal.AST
  ( QUERY,
    TypeCategory (..),
    TypeName,
    TypeWrapper (..),
    mkBaseType,
    packName,
    unpackName,
  )
import Data.Text
  ( intercalate,
    pack,
    unpack,
  )
import Data.Typeable
  ( TyCon,
    TypeRep,
    splitTyConApp,
    tyConFingerprint,
    tyConName,
    typeRep,
    typeRepTyCon,
  )
import Relude hiding (Undefined, intercalate)

resolverCon :: TyCon
resolverCon = typeRepTyCon $ typeRep $ Proxy @(Resolver QUERY () Maybe)

-- Ignores Resolver name  from typeName
ignoreResolver :: (TyCon, [TypeRep]) -> [TyCon]
ignoreResolver (con, _) | con == resolverCon = []
ignoreResolver (con, args) =
  con : concatMap (ignoreResolver . splitTyConApp) args

getTypeConstructors :: Typeable a => f a -> [TyCon]
getTypeConstructors = ignoreResolver . splitTyConApp . typeRep

getTypeConstructorNames :: Typeable a => f a -> [Text]
getTypeConstructorNames = fmap (pack . tyConName . replacePairCon) . getTypeConstructors

getTypename :: Typeable a => f a -> TypeName
getTypename = packName . intercalate "" . getTypeConstructorNames

-- | replaces typeName (A,B) with Pair_A_B
replacePairCon :: TyCon -> TyCon
replacePairCon x | hsPair == x = gqlPair
  where
    hsPair = typeRepTyCon $ typeRep $ Proxy @(Int, Int)
    gqlPair = typeRepTyCon $ typeRep $ Proxy @(Pair Int Int)
replacePairCon x = x

getFingerprint :: Typeable a => TypeCategory -> f a -> TypeFingerprint
getFingerprint category = TypeableFingerprint category . fmap tyConFingerprint . getTypeConstructors

deriveTypeData :: Typeable a => f a -> (Bool -> String -> String) -> TypeCategory -> TypeData
deriveTypeData proxy typeNameModifier cat =
  TypeData
    { gqlTypeName = packName . pack $ typeNameModifier (cat == IN) originalTypeName,
      gqlWrappers = mkBaseType,
      gqlFingerprint = getFingerprint cat proxy
    }
  where
    originalTypeName = unpack . unpackName $ getTypename proxy

data TypeData = TypeData
  { gqlTypeName :: TypeName,
    gqlWrappers :: TypeWrapper,
    gqlFingerprint :: TypeFingerprint
  }
  deriving (Show)

mkTypeData :: TypeName -> a -> TypeData
mkTypeData name _ =
  TypeData
    { gqlTypeName = name,
      gqlFingerprint = InternalFingerprint name,
      gqlWrappers = mkBaseType
    }
