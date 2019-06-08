{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Morpheus.Types.GQLType
  ( GQLType(..)
  , scalarTypeOf
  , asObjectType
  , enumTypeOf
  , inputObjectOf
  ) where

import           Data.Morpheus.Error.Schema                        (nameCollisionError)
import           Data.Morpheus.Resolve.Generics.TypeRep            (TypeUpdater, resolveTypes)
import           Data.Morpheus.Schema.Directive                    (Directive)
import           Data.Morpheus.Schema.DirectiveLocation            (DirectiveLocation)
import           Data.Morpheus.Schema.EnumValue                    (EnumValue)
import           Data.Morpheus.Schema.Internal.RenderIntrospection (Field, InputValue, Type)
import           Data.Morpheus.Schema.Schema                       (Schema)
import           Data.Morpheus.Schema.TypeKind                     (TypeKind (..))
import           Data.Morpheus.Types.Internal.Data                 (DataField (..), DataFullType (..), DataInputField,
                                                                    DataLeaf (..), DataOutputField, DataType (..),
                                                                    DataTypeWrapper (..), DataValidator, defineType,
                                                                    isTypeDefined)
import           Data.Morpheus.Types.Resolver                      ((::->))
import           Data.Proxy                                        (Proxy (..))
import           Data.Text                                         (Text, pack)
import           Data.Typeable                                     (Typeable, tyConName, typeRep, typeRepFingerprint,
                                                                    typeRepTyCon)
import           GHC.Fingerprint.Type                              (Fingerprint)

scalarTypeOf :: GQLType a => DataValidator -> Proxy a -> DataFullType
scalarTypeOf validator = Leaf . LeafScalar . buildType validator

enumTypeOf :: GQLType a => [Text] -> Proxy a -> DataFullType
enumTypeOf tags' = Leaf . LeafEnum . buildType tags'

asObjectType :: GQLType a => [(Text, DataOutputField)] -> Proxy a -> DataFullType
asObjectType fields' = OutputObject . buildType fields'

inputObjectOf :: GQLType a => [(Text, DataInputField)] -> Proxy a -> DataFullType
inputObjectOf fields' = InputObject . buildType fields'

class GQLType a where
  description :: Proxy a -> Text
  description _ = ""
  __typeName :: Proxy a -> Text
  default __typeName :: (Typeable a) =>
    Proxy a -> Text
  __typeName _ = pack $ tyConName $ typeRepTyCon $ typeRep $ Proxy @a
  __typeFingerprint :: Proxy a -> Fingerprint
  default __typeFingerprint :: (Typeable a) =>
    Proxy a -> Fingerprint
  __typeFingerprint = typeRepFingerprint . typeRep
  field_ :: TypeKind -> Proxy a -> t -> Text -> DataField t
  field_ kind' proxy' args' name' =
    DataField
      { fieldName = name'
      , fieldTypeWrappers = [NonNullType]
      , fieldKind = kind'
      , fieldType = __typeName proxy'
      , fieldArgs = args'
      }
  buildType :: t -> Proxy a -> DataType t
  buildType typeData' proxy =
    DataType
      { typeName = __typeName proxy
      , typeFingerprint = __typeFingerprint proxy
      , typeDescription = description proxy
      , typeData = typeData'
      }
  updateLib :: (Proxy a -> DataFullType) -> [TypeUpdater] -> Proxy a -> TypeUpdater
  updateLib typeBuilder stack proxy lib' =
    case isTypeDefined (__typeName proxy) lib' of
      Nothing -> resolveTypes (defineType (__typeName proxy, typeBuilder proxy) lib') stack
      Just fingerprint'
        | fingerprint' == __typeFingerprint proxy -> return lib'
      Just _ -> Left $ nameCollisionError (__typeName proxy)

instance GQLType EnumValue where
  __typeName _ = "__EnumValue"

instance GQLType Type where
  __typeName _ = "__Type"

instance GQLType Field where
  __typeName = const "__Field"

instance GQLType InputValue where
  __typeName = const "__InputValue"

instance GQLType Schema where
  __typeName _ = "__Schema"

instance GQLType Directive where
  __typeName _ = "__Directive"

instance GQLType TypeKind where
  __typeName _ = "__TypeKind"

instance GQLType DirectiveLocation where
  __typeName _ = "__DirectiveLocation"

instance GQLType Int where
  __typeName _ = "Int"

instance GQLType Float where
  __typeName _ = "Float"

instance GQLType Text where
  __typeName _ = "String"

instance GQLType Bool where
  __typeName _ = "Boolean"

instance GQLType a => GQLType (Maybe a) where
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)

instance GQLType a => GQLType [a] where
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)

instance GQLType a => GQLType (p ::-> a) where
  __typeName _ = __typeName (Proxy @a)
  __typeFingerprint _ = __typeFingerprint (Proxy @a)
