{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Kind.GQLInputObject
  ( decode
  , inputField
  , introspect
  , IObjectConstraint
  ) where

import           Data.Morpheus.Error.Internal        (internalTypeMismatch)
import           Data.Morpheus.Generics.GDecode      (GDecode (..))
import           Data.Morpheus.Generics.ObjectRep    (ObjectRep (..))
import           Data.Morpheus.Kind.GQLType          (GQLType (..), inputObjectOf)
import           Data.Morpheus.Schema.Internal.Types (InputField (..), TypeLib)
import           Data.Morpheus.Schema.TypeKind       (TypeKind (..))
import           Data.Morpheus.Types.Error           (Validation)
import           Data.Morpheus.Types.JSType          (JSType (..))
import           Data.Proxy                          (Proxy (..))
import           Data.Text                           (Text)
import           GHC.Generics

type IOObjectRep a = ObjectRep (Rep a) (Text, InputField)

type IObjectConstraint a = (GQLType a, Generic a, GDecode JSType (Rep a), IOObjectRep a)

decode :: (Generic a, GDecode JSType (Rep a)) => JSType -> Validation a
decode (JSObject x) = to <$> gDecode "" (JSObject x)
decode isType       = internalTypeMismatch "InputObject" isType

inputField :: GQLType a => Proxy a -> Text -> InputField
inputField proxy = InputField . field_ INPUT_OBJECT proxy

introspect ::
     forall a. (GQLType a, IOObjectRep a)
  => Proxy a
  -> TypeLib
  -> TypeLib
introspect = updateLib (inputObjectOf fields) stack
  where
    fieldTypes = getFields (Proxy @(Rep a))
    stack = map snd fieldTypes
    fields = map fst fieldTypes
