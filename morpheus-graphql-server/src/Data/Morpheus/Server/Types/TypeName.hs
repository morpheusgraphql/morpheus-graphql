{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Types.TypeName
  ( getTypename,
    getTypeConstructorNames,
    getFingerprint,
    TypeFingerprint (..),
  )
where

-- MORPHEUS

import Data.Data (tyConFingerprint)
import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (CatType (..))
import Data.Morpheus.Server.NamedResolvers (NamedResolverT (..))
import Data.Morpheus.Server.Types.Types
  ( Pair,
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeCategory (..),
    TypeName,
    packName,
  )
import Data.Text
  ( intercalate,
    pack,
  )
import Data.Typeable
  ( TyCon,
    TypeRep,
    splitTyConApp,
    tyConName,
    typeRep,
    typeRepTyCon,
  )
import GHC.Fingerprint
import Relude hiding (Seq, Undefined, intercalate)

data TypeFingerprint
  = TypeableFingerprint TypeCategory [Fingerprint]
  | InternalFingerprint TypeName
  | CustomFingerprint TypeName
  deriving
    ( Generic,
      Show,
      Eq,
      Ord
    )

getTypename :: Typeable a => f a -> TypeName
getTypename = packName . intercalate "" . getTypeConstructorNames

getTypeConstructorNames :: Typeable a => f a -> [Text]
getTypeConstructorNames = fmap (pack . tyConName . replacePairCon) . getTypeConstructors

getTypeConstructors :: Typeable a => f a -> [TyCon]
getTypeConstructors = ignoreResolver . splitTyConApp . typeRep

-- | replaces typeName (A,B) with Pair_A_B
replacePairCon :: TyCon -> TyCon
replacePairCon x | hsPair == x = gqlPair
  where
    hsPair = typeRepTyCon $ typeRep $ Proxy @(Int, Int)
    gqlPair = typeRepTyCon $ typeRep $ Proxy @(Pair Int Int)
replacePairCon x = x

-- Ignores Resolver name  from typeName
ignoreResolver :: (TyCon, [TypeRep]) -> [TyCon]
ignoreResolver (con, _) | con == typeRepTyCon (typeRep $ Proxy @Resolver) = []
ignoreResolver (con, _) | con == typeRepTyCon (typeRep $ Proxy @NamedResolverT) = []
ignoreResolver (con, args) =
  con : concatMap (ignoreResolver . splitTyConApp) args

getFingerprint :: Typeable a => CatType c a -> TypeFingerprint
getFingerprint p@InputType = TypeableFingerprint IN $ tyConFingerprint <$> getTypeConstructors p
getFingerprint p@OutputType = TypeableFingerprint OUT $ tyConFingerprint <$> getTypeConstructors p
