{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Server.Deriving.Utils
  ( EnumRep (..),
    datatypeNameProxy,
    conNameProxy,
    isRecordProxy,
    selNameProxy,
    withNamespace,
  )
where

import Data.Morpheus.Internal.Utils (stripNamespace)
import Data.Morpheus.Server.Types.GQLType (GQLType (hasNamespace))
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    FieldName (..),
    TypeName (..),
    convertToJSONName,
  )
import Data.Proxy (Proxy (..))
import Data.Text
  ( pack,
  )
import GHC.Generics

-- MORPHEUS
class EnumRep (f :: * -> *) where
  enumTags :: Proxy f -> [TypeName]

instance EnumRep f => EnumRep (M1 D c f) where
  enumTags _ = enumTags (Proxy @f)

instance (Constructor c) => EnumRep (M1 C c U1) where
  enumTags _ = [conNameProxy (Proxy @c)]

instance (EnumRep a, EnumRep b) => EnumRep (a :+: b) where
  enumTags _ = enumTags (Proxy @a) ++ enumTags (Proxy @b)

datatypeNameProxy :: forall f (d :: Meta). Datatype d => f d -> TypeName
datatypeNameProxy _ = TypeName $ pack $ datatypeName (undefined :: (M1 D d f a))

conNameProxy :: forall f (c :: Meta). Constructor c => f c -> TypeName
conNameProxy _ = TypeName $ pack $ conName (undefined :: M1 C c U1 a)

selNameProxy :: forall f (s :: Meta). Selector s => f s -> FieldName
selNameProxy _ = convertToJSONName $ FieldName $ pack $ selName (undefined :: M1 S s f a)

withNamespace :: forall f a. GQLType a => f a -> FieldName -> FieldName
withNamespace _ = stripNamespace (hasNamespace (Proxy @a))

isRecordProxy :: forall f (c :: Meta). Constructor c => f c -> Bool
isRecordProxy _ = conIsRecord (undefined :: (M1 C c f a))
