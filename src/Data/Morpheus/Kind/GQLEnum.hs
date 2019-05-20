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
  , introspect
  , encode
  ) where

import           Data.Morpheus.Generics.EnumRep         (EnumRep (..))
import           Data.Morpheus.Kind.GQLType             (GQLType (..), enumTypeOf)
import           Data.Morpheus.Kind.Internal            (ENUM, KIND)
import           Data.Morpheus.Schema.DirectiveLocation (DirectiveLocation)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import           Data.Morpheus.Types.Internal.AST       (ASTTypeLib)
import           Data.Morpheus.Types.Internal.Value     (ScalarValue (..), Value (..))
import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)
import           GHC.Generics

type EnumConstraint a = (Generic a, EnumRep (Rep a), GQLType a)

decode :: (Generic a, EnumRep (Rep a)) => Text -> a
decode text = to $ gToEnum text

encode :: (Generic a, EnumRep (Rep a)) => a -> Value
encode = Scalar . String . encodeRep . from

introspect ::
     forall a. (GQLType a, EnumRep (Rep a))
  => Proxy a
  -> ASTTypeLib
  -> ASTTypeLib
introspect = updateLib (enumTypeOf $ getTags (Proxy @(Rep a))) []

type instance KIND TypeKind = ENUM

type instance KIND DirectiveLocation = ENUM
