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

import           Data.Morpheus.Error.Internal       (internalTypeMismatch)
import           Data.Morpheus.Generics.GDecode     (GDecode (..))
import           Data.Morpheus.Generics.ObjectRep   (ObjectRep (..))
import           Data.Morpheus.Kind.GQLType         (GQLType (..), inputObjectOf)
import           Data.Morpheus.Schema.TypeKind      (TypeKind (..))
import           Data.Morpheus.Types.Error          (Validation)
import           Data.Morpheus.Types.Internal.Data   (DataInputField, DataTypeLib)
import           Data.Morpheus.Types.Internal.Value (Value (..))
import           Data.Proxy                         (Proxy (..))
import           Data.Text                          (Text)
import           GHC.Generics

type IOObjectRep a = ObjectRep (Rep a) (Text, DataInputField)

type IObjectConstraint a = (GQLType a, Generic a, GDecode Value (Rep a), IOObjectRep a)

decode :: (Generic a, GDecode Value (Rep a)) => Value -> Validation a
decode (Object x) = to <$> gDecode "" (Object x)
decode isType     = internalTypeMismatch "InputObject" isType

inputField :: GQLType a => Proxy a -> Text -> DataInputField
inputField proxy = field_ INPUT_OBJECT proxy ()

introspect ::
     forall a. (GQLType a, IOObjectRep a)
  => Proxy a
  -> DataTypeLib
  -> DataTypeLib
introspect = updateLib (inputObjectOf fields') stack'
  where
    (fields', stack') = unzip $ getFields (Proxy @(Rep a))
