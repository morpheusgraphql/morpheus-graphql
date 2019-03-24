{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Morpheus.Kind.GQLEnum
  ( GQLEnum(decode, introspect, enumType, fieldType)
  ) where

import qualified Data.Data                              as D
import qualified Data.Map                               as M
import           Data.Morpheus.Generics.GDecodeEnum     (GDecodeEnum (..))
import           Data.Morpheus.Kind.GQLKind             (GQLKind (..), enumTypeOf)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.Field             (createFieldWith)
import           Data.Morpheus.Schema.InputValue        (createInputValueWith)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Schema.Utils.Utils       (Field, InputValue, TypeLib)
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           GHC.Generics

class GQLEnum a where
  decode :: Text -> a
  default decode :: (Show a, Generic a, D.Data a, GDecodeEnum (Rep a)) =>
    Text -> a
  decode text = to $ gToEnum text
  enumType :: Proxy a -> T.Text -> InputValue
  default enumType :: (Show a, GQLKind a) =>
    Proxy a -> T.Text -> InputValue
  enumType proxy name = createInputValueWith name (enumTypeOf proxy [])
  fieldType :: Proxy a -> T.Text -> Field
  default fieldType :: (Show a, GQLKind a) =>
    Proxy a -> T.Text -> Field
  fieldType proxy name = createFieldWith name (enumTypeOf proxy []) []
  introspect :: Proxy a -> TypeLib -> TypeLib
  default introspect :: (Show a, GQLKind a, GDecodeEnum (Rep a)) =>
    Proxy a -> TypeLib -> TypeLib
  introspect proxy typeLib =
    case M.lookup typeName typeLib of
      Just _  -> typeLib
      Nothing -> M.insert typeName (enumTypeOf proxy tags) typeLib
    where
      typeName = typeID proxy
      tags = getTags (Proxy @(Rep a))

instance GQLEnum TypeKind

instance GQLEnum DirectiveLocation
