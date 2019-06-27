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

import           Data.Morpheus.Types.Internal.Data (DataFingerprint (..))
import           Data.Morpheus.Types.Resolver      ((::->), Resolver)
import           Data.Proxy                        (Proxy (..))
import           Data.Text                         (Text, intercalate, pack)
import           Data.Typeable                     (TyCon, TypeRep, Typeable, splitTyConApp, tyConFingerprint,
                                                    tyConName, typeRep)

resolverCon :: TyCon
resolverCon = fst $ splitTyConApp $ typeRep $ Proxy @(Resolver Maybe)

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
  __typeName :: Proxy a -> Text
  default __typeName :: (Typeable a) =>
    Proxy a -> Text
  __typeName _ = intercalate "_" (getName $ Proxy @a)
    where
      getName = fmap (map (pack . tyConName)) (ignoreResolver . splitTyConApp . typeRep)
  __typeFingerprint :: Proxy a -> DataFingerprint
  default __typeFingerprint :: (Typeable a) =>
    Proxy a -> DataFingerprint
  __typeFingerprint _ = TypeableFingerprint $ conFingerprints (Proxy @a)
    where
      conFingerprints = fmap (map tyConFingerprint) (ignoreResolver . splitTyConApp . typeRep)

instance GQLType Int where
  __typeName = const "Int"

instance GQLType Float where
  __typeName = const "Float"

instance GQLType Text where
  __typeName = const "String"

instance GQLType Bool where
  __typeName = const "Boolean"

instance GQLType a => GQLType (Maybe a) where
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)

instance GQLType a => GQLType [a] where
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)

instance GQLType a => GQLType (p ::-> a) where
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)
