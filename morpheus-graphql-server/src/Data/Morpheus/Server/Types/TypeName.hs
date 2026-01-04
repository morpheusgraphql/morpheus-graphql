{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
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
    tyConName,
    typeRep,
    typeRepTyCon,
  )
import GHC.Fingerprint
import Relude hiding (Seq, Show, Undefined, intercalate, show)
import Prelude (Show (..))

data TypeFingerprint
  = TypeableFingerprint
      { category :: TypeCategory,
        fingerprints :: [Fingerprint]
      }
  | InternalFingerprint TypeName
  | CustomFingerprint TypeName
  deriving
    ( Generic,
      Eq,
      Ord
    )

instance Show TypeFingerprint where
  show TypeableFingerprint {..} = "TYPEABLE:" <> unpack (intercalate ":" (pack (show category) : map (pack . show) fingerprints))
  show (InternalFingerprint name) = "INTERNAL:" <> unpack (unpackName name)
  show (CustomFingerprint name) = "CUSTOM:" <> unpack (unpackName name)

instance Hashable TypeFingerprint where
  hashWithSalt s TypeableFingerprint {..} = hashWithSalt s (1 :: Int, category, map show fingerprints)
  hashWithSalt s (InternalFingerprint x) = hashWithSalt s (2 :: Int, x)
  hashWithSalt s (CustomFingerprint x) = hashWithSalt s (3 :: Int, x)

typeableTypename :: (Typeable a) => f a -> TypeName
typeableTypename = packName . intercalate "" . fmap (pack . dropLeadingTick . tyConName . replacePairCon) . getTypeConstructors

toCategory :: CatType c a -> TypeCategory
toCategory InputType = IN
toCategory OutputType = OUT

typeableFingerprint :: (Typeable a) => CatType c a -> TypeFingerprint
typeableFingerprint p =
  TypeableFingerprint
    { category = toCategory p,
      fingerprints = tyConFingerprint <$> getTypeConstructors p
    }

getTypeConstructors :: (Typeable a) => f a -> [TyCon]
getTypeConstructors = ignoreResolver . splitTyConApp . typeRep

-- Filter out ticks from promoted constructors
dropLeadingTick :: String -> String
dropLeadingTick = Prelude.dropWhile (== '\'') 

rep :: forall k (a :: k) f. (Typeable a) => f a -> TyCon
rep = typeRepTyCon . typeRep

-- | replaces typeName (A,B) with Pair_A_B
replacePairCon :: TyCon -> TyCon
replacePairCon x | rep (Proxy @(Int, Int)) == x = rep (Proxy @(Pair Int Int))
replacePairCon x = x

ignoredTypes :: [TyCon]
ignoredTypes =
  [ rep (Proxy @Resolver),
    rep (Proxy @NamedResolverT)
  ]

-- ignores resolver names from typename
ignoreResolver :: (TyCon, [TypeRep]) -> [TyCon]
ignoreResolver (con, _) | con `elem` ignoredTypes = []
ignoreResolver (con, args) = con : concatMap (ignoreResolver . splitTyConApp) args
