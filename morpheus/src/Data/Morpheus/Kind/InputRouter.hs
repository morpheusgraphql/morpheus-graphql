{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Data.Morpheus.Kind.InputRouter where

import           Data.Morpheus.Kind.Internal (Decode_, GQL, IField_, Intro_)
import           Data.Proxy                  (Proxy (..))

class InputTypeRouter a b where
  __introspect :: Proxy b -> Intro_ a
  __decode :: Proxy b -> Decode_ a
  __field :: Proxy b -> IField_ a

_field ::
     forall a. InputTypeRouter a (GQL a)
  => IField_ a
_field = __field (Proxy @(GQL a))

_decode ::
     forall a. InputTypeRouter a (GQL a)
  => Decode_ a
_decode = __decode (Proxy @(GQL a))

_introspect ::
     forall a. InputTypeRouter a (GQL a)
  => Intro_ a
_introspect = __introspect (Proxy @(GQL a))
