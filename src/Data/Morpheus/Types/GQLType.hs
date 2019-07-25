{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Morpheus.Types.GQLType
  ( GQLType(..)
  ) where

import           Data.Proxy                        (Proxy (..))
import           Data.Set                          (Set)
import           Data.Text                         (Text, intercalate, pack)
import           Data.Typeable                     (TyCon, TypeRep, Typeable, splitTyConApp, tyConFingerprint,
                                                    tyConName, typeRep)

-- MORPHEUS
import           Data.Morpheus.Types.Custom        (MapKind, Pair)
import           Data.Morpheus.Types.Internal.Data (DataFingerprint (..))
import           Data.Morpheus.Types.Resolver      (Resolver)

resolverCon :: TyCon
resolverCon = fst $ splitTyConApp $ typeRep $ Proxy @(Resolver Maybe)

-- | replaces typeName (A,B) with Pair_A_B
replacePairCon :: TyCon -> TyCon
replacePairCon x
  | hsPair == x = gqlPair
  where
    hsPair = fst $ splitTyConApp $ typeRep $ Proxy @(Int, Int)
    gqlPair = fst $ splitTyConApp $ typeRep $ Proxy @(Pair Int Int)
replacePairCon x = x

-- Ignores Resolver name  from typeName
ignoreResolver :: (TyCon, [TypeRep]) -> [TyCon]
ignoreResolver (con, _)
  | con == resolverCon = []
ignoreResolver (con, args) = con : concatMap (ignoreResolver . splitTyConApp) args

-- | GraphQL type, every graphQL type should have an instance of 'GHC.Generics.Generic' and 'GQLType'.
--
--  @
--    ... deriving (Generic, GQLType)
--  @
--
-- if you want to add description
--
--  @
--       ... deriving (Generic)
--
--     instance GQLType ... where
--       description = const "your description ..."
--  @
class GQLType a where
  description :: Proxy a -> Text
  description _ = ""
  __typeVisibility :: Proxy a -> Bool
  __typeVisibility = const True
  __typeName :: Proxy a -> Text
  default __typeName :: (Typeable a) =>
    Proxy a -> Text
  __typeName _ = intercalate "_" (getName $ Proxy @a)
    where
      getName = fmap (map (pack . tyConName)) (map replacePairCon . ignoreResolver . splitTyConApp . typeRep)
  __typeFingerprint :: Proxy a -> DataFingerprint
  default __typeFingerprint :: (Typeable a) =>
    Proxy a -> DataFingerprint
  __typeFingerprint _ = TypeableFingerprint $ conFingerprints (Proxy @a)
    where
      conFingerprints = fmap (map tyConFingerprint) (ignoreResolver . splitTyConApp . typeRep)

instance GQLType Int where
  __typeVisibility = const False

instance GQLType Float where
  __typeVisibility = const False

instance GQLType Text where
  __typeName = const "String"
  __typeVisibility = const False

instance GQLType Bool where
  __typeName = const "Boolean"
  __typeVisibility = const False

instance GQLType a => GQLType (Maybe a) where
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)

instance GQLType a => GQLType [a] where
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)

instance GQLType a => GQLType (Set a) where
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (a, b) where
  __typeName _ = __typeName $ Proxy @(Pair a b)

instance (Typeable a, Typeable b, GQLType a, GQLType b) => GQLType (Pair a b)

instance (Typeable a, Typeable b, Typeable m, GQLType a, GQLType b) => GQLType (MapKind a b m)
