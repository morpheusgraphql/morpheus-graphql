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
import           Data.Morpheus.Kind.GQLKind             (GQLKind (..))
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import qualified Data.Morpheus.Schema.Field             as F (createFieldWith)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import qualified Data.Morpheus.Schema.Utils.Utils       as I (Field, InputValue, TypeLib,
                                                              createEnum, createInputValue)
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           GHC.Generics

class GQLEnum a where
  decode :: Text -> a
  default decode :: (Show a, Generic a, D.Data a, GDecodeEnum (Rep a)) =>
    Text -> a
  decode text = to $ gToEnum text
  enumType :: Proxy a -> T.Text -> I.InputValue
  default enumType :: (Show a, GQLKind a) =>
    Proxy a -> T.Text -> I.InputValue
  enumType proxy name = I.createInputValue name $ typeID proxy
  fieldType :: Proxy a -> T.Text -> I.Field
  default fieldType :: (Show a, GQLKind a) =>
    Proxy a -> T.Text -> I.Field
  fieldType proxy name = F.createFieldWith name (I.createEnum (typeID proxy) []) []
  introspect :: Proxy a -> I.TypeLib -> I.TypeLib
  default introspect :: (Show a, GQLKind a, GDecodeEnum (Rep a)) =>
    Proxy a -> I.TypeLib -> I.TypeLib
  introspect proxy typeLib =
    case M.lookup typeName typeLib of
      Just _  -> typeLib
      Nothing -> M.insert typeName (I.createEnum typeName tags) typeLib
    where
      typeName = typeID proxy
      tags = getTags (Proxy @(Rep a))

instance GQLEnum TypeKind

instance GQLEnum DirectiveLocation
