{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Data.Morpheus.Kind.OutputRouter where

import           Data.Morpheus.Kind.Internal (Encode_, GQL, Intro_, OField_)
import           Data.Proxy                  (Proxy (..))

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
