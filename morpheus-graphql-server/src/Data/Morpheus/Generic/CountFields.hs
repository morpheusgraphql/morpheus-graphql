{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Generic.CountFields (CountFields (..)) where

import GHC.Generics
import Relude

class CountFields (f :: Type -> Type) where
  countFields :: Proxy f -> Int

instance (CountFields f, CountFields g) => CountFields (f :*: g) where
  countFields _ = countFields (Proxy @f) + countFields (Proxy @g)

instance (Selector s) => CountFields (M1 S s (K1 i a)) where
  countFields _ = 1

instance CountFields U1 where
  countFields _ = 0
