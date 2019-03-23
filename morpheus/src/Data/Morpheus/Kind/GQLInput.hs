{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Data.Morpheus.Kind.GQLInput
  ( GQLInput(..)
  ) where

import           Data.Data                        (Data, Typeable)
import qualified Data.Map                         as M
import           Data.Morpheus.Error.Internal     (internalArgumentError, internalTypeMismatch)
import           Data.Morpheus.Generics.GDecode   (GDecode (..))
import           Data.Morpheus.Generics.TypeRep   (Selectors (..), resolveTypes)
import qualified Data.Morpheus.Kind.GQLEnum       as E (GQLEnum (..))
import           Data.Morpheus.Kind.GQLKind       (GQLKind (..))
import qualified Data.Morpheus.Schema.InputValue  as I (InputValue (..))
import           Data.Morpheus.Schema.Utils.Utils (Field, InputValue, TypeLib, createInputObject,
                                                   createInputValue)
import           Data.Morpheus.Types.Describer    (EnumOf (..))
import           Data.Morpheus.Types.Error        (Validation)
import           Data.Morpheus.Types.JSType       (JSType (..))
import qualified Data.Morpheus.Types.MetaInfo     as Meta (MetaInfo (..), initialMeta)
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text)
import           GHC.Generics

instance GQLInput a => GDecode JSType (K1 i a) where
  gDecode meta (JSObject object) =
    case lookup (Meta.key meta) object of
      Nothing    -> internalArgumentError "Missing Argument"
      Just value -> K1 <$> decode value
  gDecode _ isType = internalTypeMismatch "InputObject" isType

class GQLInput a where
  decode :: JSType -> Validation a
  default decode :: (Show a, Generic a, Data a, GDecode JSType (Rep a)) =>
    JSType -> Validation a
  decode (JSObject x) = to <$> gDecode Meta.initialMeta (JSObject x)
  decode isType       = internalTypeMismatch "InputObject" isType
  typeInfo :: Proxy a -> Text -> InputValue
  default typeInfo :: (Show a, GQLKind a) =>
    Proxy a -> Text -> InputValue
  typeInfo proxy name = createInputValue name $ typeID proxy
  introInput :: Proxy a -> TypeLib -> TypeLib
  default introInput :: (Show a, GQLKind a, Selectors (Rep a) Field) =>
    Proxy a -> TypeLib -> TypeLib
  introInput proxy typeLib =
    case M.lookup typeName typeLib of
      Just _  -> typeLib
      Nothing -> addType
    where
      addType = resolveTypes (M.insert typeName (createInputObject typeName gqlFields) typeLib) stack
      typeName = typeID proxy
      fieldTypes = getFields (Proxy @(Rep a))
      stack = map snd fieldTypes
      gqlFields = map fst fieldTypes

instance GQLInput Text where
  decode (JSString x) = pure x
  decode isType       = internalTypeMismatch "String" isType
  typeInfo _ name = createInputValue name "String"
  introInput _ typeLib = typeLib

instance GQLInput Bool where
  decode (JSBool x) = pure x
  decode isType     = internalTypeMismatch "Boolean" isType
  typeInfo _ name = createInputValue name "Boolean"
  introInput _ typeLib = typeLib

instance GQLInput Int where
  decode (JSInt x) = pure x
  decode isType    = internalTypeMismatch "Int" isType
  typeInfo _ name = createInputValue name "Int"
  introInput _ typeLib = typeLib

instance (GQLInput a, Show a, Typeable a) => GQLInput (Maybe a) where
  decode JSNull = pure Nothing
  decode x      = Just <$> decode x
  typeInfo _ name = (typeInfo (Proxy @a) name) {I.defaultValue = "Nothing"}
  introInput _ typeLib = typeLib

instance (Show a, E.GQLEnum a) => GQLInput (EnumOf a) where
  decode (JSEnum text) = pure $ EnumOf (E.decode text)
  decode isType        = internalTypeMismatch "Enum" isType
  typeInfo _ = E.enumType (Proxy @a)
  introInput _ = E.introspect (Proxy @a)

instance (Show a, GQLInput a) => GQLInput [a] where
  decode (JSList li) = mapM decode li
  decode isType      = internalTypeMismatch "List" isType
  typeInfo _ = typeInfo (Proxy @a)
  introInput _ = introInput (Proxy @a) -- wrap as List
