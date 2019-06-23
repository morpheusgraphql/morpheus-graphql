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

import           Data.Morpheus.Schema.Directive                    (Directive)
import           Data.Morpheus.Schema.DirectiveLocation            (DirectiveLocation)
import           Data.Morpheus.Schema.EnumValue                    (EnumValue)
import           Data.Morpheus.Schema.Internal.RenderIntrospection (Field, InputValue, Type)
import           Data.Morpheus.Schema.Schema                       (Schema)
import           Data.Morpheus.Schema.TypeKind                     (TypeKind (..))
import           Data.Morpheus.Types.Resolver                      ((::->))
import           Data.Proxy                                        (Proxy (..))
import           Data.Text                                         (Text, intercalate, pack)
import           Data.Typeable                                     (Typeable, splitTyConApp, tyConName, typeRep,
                                                                    typeRepFingerprint)
import           GHC.Fingerprint.Type                              (Fingerprint)

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
  __typeName _ = generateName $ typeRep $ Proxy @a
    where
      generateName = joinWithSubTypes . splitTyConApp
        where
          joinWithSubTypes (con', args') = intercalate "_" $ pack (tyConName con') : map generateName args'
  __typeFingerprint :: Proxy a -> Fingerprint
  default __typeFingerprint :: (Typeable a) =>
    Proxy a -> Fingerprint
  __typeFingerprint = typeRepFingerprint . typeRep

instance GQLType EnumValue where
  __typeName = const "__EnumValue"

instance GQLType Type where
  __typeName = const "__Type"

instance GQLType Field where
  __typeName = const "__Field"

instance GQLType InputValue where
  __typeName = const "__InputValue"

instance GQLType Schema where
  __typeName = const "__Schema"

instance GQLType Directive where
  __typeName = const "__Directive"

instance GQLType TypeKind where
  __typeName = const "__TypeKind"

instance GQLType DirectiveLocation where
  __typeName = const "__DirectiveLocation"

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
