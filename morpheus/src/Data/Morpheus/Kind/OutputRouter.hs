{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Kind.OutputRouter where

import qualified Data.Morpheus.Kind.GQLEnum          as E (GQLEnum (..))
import           Data.Morpheus.Kind.GQLKind          (GQLKind)
import qualified Data.Morpheus.Kind.GQLPrimitive     as P (GQLPrimitive (..))
import qualified Data.Morpheus.Kind.GQLScalar        as S (GQLScalar (..))
import           Data.Morpheus.Kind.Internal         (ENUM, Encode_, GQL, Intro_, OField_, PRIMITIVE, SCALAR)
import           Data.Morpheus.Schema.Internal.Types (ObjectField (..))
import           Data.Morpheus.Types.JSType          (JSType (..), ScalarValue (..))
import           Data.Proxy                          (Proxy (..))
import           Data.Text                           (pack)

class OutputTypeRouter a b where
  __introspect :: Proxy b -> Intro_ a
  __encode :: Proxy b -> Encode_ a
  __objectField :: Proxy b -> OField_ a

_objectField ::
     forall a. OutputTypeRouter a (GQL a)
  => OField_ a
_objectField = __objectField (Proxy @(GQL a))

_introspect ::
     forall a. OutputTypeRouter a (GQL a)
  => Intro_ a
_introspect = __introspect (Proxy @(GQL a))

_encode ::
     forall a. OutputTypeRouter a (GQL a)
  => Encode_ a
_encode = __encode (Proxy @(GQL a))

instance (S.GQLScalar a, GQLKind a) => OutputTypeRouter a SCALAR where
  __introspect _ _ = S.introspect (Proxy @a)
  __encode _ _ = pure . S.encode
  __objectField _ _ = ObjectField [] . S.asField (Proxy @a)

instance (E.GQLEnum a, Show a, GQLKind a) => OutputTypeRouter a ENUM where
  __introspect _ _ = E.introspect (Proxy @a)
  __encode _ _ = pure . Scalar . String . pack . show
  __objectField _ _ = ObjectField [] . E.asField (Proxy @a)

instance (P.GQLPrimitive a, GQLKind a) => OutputTypeRouter a PRIMITIVE where
  __introspect _ = P.introspect'
  __encode _ = P.encode'
  __objectField _ = P.objectField'
