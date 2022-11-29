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
  ( typeableTypename,
    typeableFingerprint,
    TypeFingerprint (..),
  )
where

-- MORPHEUS

import Data.Data (tyConFingerprint)
import Data.Morpheus.App.Internal.Resolving
  ( Resolver,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (CatType (..))
import Data.Morpheus.Server.Types.NamedResolvers (NamedResolverT (..))
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

typeableTypename :: Typeable a => f a -> TypeName
typeableTypename = packName . intercalate "" . fmap (pack . tyConName . replacePairCon) . getTypeConstructors

typeableFingerprint :: Typeable a => CatType c a -> TypeFingerprint
typeableFingerprint p@InputType = TypeableFingerprint IN $ tyConFingerprint <$> getTypeConstructors p
typeableFingerprint p@OutputType = TypeableFingerprint OUT $ tyConFingerprint <$> getTypeConstructors p

getTypeConstructors :: Typeable a => f a -> [TyCon]
getTypeConstructors = ignoreResolver . splitTyConApp . typeRep

-- | replaces typeName (A,B) with Pair_A_B
replacePairCon :: TyCon -> TyCon
replacePairCon x | hsPair == x = gqlPair
  where
    hsPair = typeRepTyCon $ typeRep $ Proxy @(Int, Int)
    gqlPair = typeRepTyCon $ typeRep $ Proxy @(Pair Int Int)
replacePairCon x = x

-- ignores resolver names from typename
ignoreResolver :: (TyCon, [TypeRep]) -> [TyCon]
ignoreResolver (con, _) | con == typeRepTyCon (typeRep $ Proxy @Resolver) = []
ignoreResolver (con, _) | con == typeRepTyCon (typeRep $ Proxy @NamedResolverT) = []
ignoreResolver (con, args) =
  con : concatMap (ignoreResolver . splitTyConApp) args
