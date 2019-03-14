{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Data.Morpheus.Generics.GQLInput
  ( GQLInput(..)
  ) where

import           Data.Data                            (Data, Typeable)
import qualified Data.Map                             as M
import           Data.Morpheus.Error.Arguments          (requiredArgument)
import           Data.Morpheus.Generics.GDecode       (GDecode (..))
import qualified Data.Morpheus.Generics.GQLEnum       as E (GQLEnum (..))
import           Data.Morpheus.Generics.TypeRep       (Selectors (..), resolveTypes)
import           Data.Morpheus.Generics.Utils         (typeOf)
import qualified Data.Morpheus.Schema.GQL__InputValue as I (GQL__InputValue (..))
import           Data.Morpheus.Types.Introspection    (GQLTypeLib, GQL__Field, GQL__InputValue,
                                                       createInputObject, createInputValue)
import           Data.Morpheus.Types.JSType           (JSType (..))
import qualified Data.Morpheus.Types.MetaInfo         as Meta (MetaInfo (..), initialMeta)
import           Data.Morpheus.Types.Types            (EnumOf (..), Validation)
import           Data.Proxy                           (Proxy (..))
import           Data.Text                            (Text)
import           GHC.Generics

instance GQLInput a => GDecode JSType (K1 i a) where
  gDecode meta (JSObject object) =
    case lookup (Meta.key meta) object of
      -- TODO: validate it in PreProcess
      Nothing    -> Left $ requiredArgument meta
      Just value -> K1 <$> decode value

class GQLInput a where
  decode :: JSType -> Validation a
  default decode :: (Show a, Generic a, Data a, GDecode JSType (Rep a)) =>
    JSType -> Validation a
  decode (JSObject x) = to <$> gDecode Meta.initialMeta (JSObject x)
  typeInfo :: Proxy a -> Text -> GQL__InputValue
  default typeInfo :: (Show a, Typeable a) =>
    Proxy a -> Text -> GQL__InputValue
  typeInfo _ name = createInputValue name $ typeOf (Proxy @a)
  introInput :: Proxy a -> GQLTypeLib -> GQLTypeLib
  default introInput :: (Show a, Typeable a, Selectors (Rep a) GQL__Field) =>
    Proxy a -> GQLTypeLib -> GQLTypeLib
  introInput _ typeLib =
    case M.lookup typeName typeLib of
      Just _  -> typeLib
      Nothing -> addType
    where
      addType = resolveTypes (M.insert typeName (createInputObject typeName gqlFields) typeLib) stack
      typeName = typeOf (Proxy @a)
      fieldTypes = getFields (Proxy @(Rep a))
      stack = map snd fieldTypes
      gqlFields = map fst fieldTypes

instance GQLInput Text where
  decode (JSString x) = pure x
  typeInfo _ name = createInputValue name "String"
  introInput _ typeLib = typeLib

instance GQLInput Bool where
  decode (JSBool x) = pure x
  typeInfo _ name = createInputValue name "Boolean"
  introInput _ typeLib = typeLib

instance GQLInput Int where
  decode (JSInt x) = pure x
  typeInfo _ name = createInputValue name "Int"
  introInput _ typeLib = typeLib

instance (GQLInput a, Show a, Typeable a) => GQLInput (Maybe a) where
  decode JSNull = pure Nothing
  decode x      = Just <$> decode x
  typeInfo _ name = (typeInfo (Proxy @a) name) {I.defaultValue = "Nothing"}
  introInput _ typeLib = typeLib

instance (Show a, E.GQLEnum a) => GQLInput (EnumOf a) where
  decode (JSEnum text) = pure $ EnumOf (E.decode (JSEnum text))
  typeInfo _ = E.enumType (Proxy @a)
  introInput _ = E.introspect (Proxy @a)
