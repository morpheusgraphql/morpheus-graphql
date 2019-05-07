{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}

module Data.Morpheus.Kind.GQLEnum
  ( EnumConstraint
  , decode
  , inputField
  , field
  , introspect
  , encode
  ) where

import           Data.Morpheus.Generics.EnumRep         (EnumRep (..))
import           Data.Morpheus.Kind.GQLType             (GQLType (..), enumTypeOf)
import           Data.Morpheus.Kind.Internal            (ENUM, KIND)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.Internal.Types    (Field (..), InputField (..), TypeLib)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Types.JSType             (JSType (..), ScalarValue (..))
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)
import           GHC.Generics

type EnumConstraint a = (Generic a, EnumRep (Rep a), GQLType a)

decode :: (Generic a, EnumRep (Rep a)) => Text -> a
decode text = to $ gToEnum text

encode :: (Generic a, EnumRep (Rep a)) => a -> JSType
encode = Scalar . String . encodeRep . from

inputField :: GQLType a => Proxy a -> Text -> InputField
inputField proxy = InputField . field proxy

field :: GQLType a => Proxy a -> Text -> Field
field = buildField ENUM

introspect ::
     forall a. (GQLType a, EnumRep (Rep a))
  => Proxy a
  -> TypeLib
  -> TypeLib
introspect = updateLib (enumTypeOf $ getTags (Proxy @(Rep a))) []

type instance KIND TypeKind = ENUM

type instance KIND DirectiveLocation = ENUM
