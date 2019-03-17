{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Morpheus.Generics.GQLEnum
  ( GQLEnum(decode, introspect, enumType, fieldType)
  ) where

import qualified Data.Data                              as D
import qualified Data.Map                               as M
import           Data.Morpheus.Generics.GDecodeEnum     (GDecodeEnum (..))
import           Data.Morpheus.Generics.Utils           (typeOf)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation (..))
import qualified Data.Morpheus.Schema.Field             as F (createFieldWith)
import qualified Data.Morpheus.Types.Introspection      as I
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Proxy                             (Proxy (..))
import qualified Data.Text                              as T
import           GHC.Generics

class GQLEnum a where
  decode :: JSType -> a
  default decode :: (Show a, Generic a, D.Data a, GDecodeEnum (Rep a)) =>
    JSType -> a
  decode (JSEnum text) = to $ gToEnum text
  typeID :: Proxy a -> T.Text
  default typeID :: D.Typeable a =>
    Proxy a -> T.Text
  typeID _ = typeOf (Proxy @a)
  enumType :: Proxy a -> T.Text -> I.GQL__InputValue
  default enumType :: (Show a, D.Typeable a) =>
    Proxy a -> T.Text -> I.GQL__InputValue
  enumType proxy name = I.createInputValue name $ typeID proxy
  fieldType :: Proxy a -> T.Text -> I.GQL__Field
  default fieldType :: (Show a, D.Typeable a) =>
    Proxy a -> T.Text -> I.GQL__Field
  fieldType proxy name = F.createFieldWith name (I.createEnum (typeID proxy) []) []
  introspect :: Proxy a -> I.GQLTypeLib -> I.GQLTypeLib
  default introspect :: (Show a, D.Typeable a, GDecodeEnum (Rep a)) =>
    Proxy a -> I.GQLTypeLib -> I.GQLTypeLib
  introspect proxy typeLib =
    case M.lookup typeName typeLib of
      Just _  -> typeLib
      Nothing -> M.insert typeName (I.createEnum typeName tags) typeLib
    where
      typeName = typeID proxy
      tags = getTags (Proxy @(Rep a))

instance GQLEnum I.GQL__TypeKind where
  typeID _ = "__TypeKind"

instance GQLEnum GQL__DirectiveLocation where
  typeID _ = "__DirectiveLocation"
