{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Generic.RefType
  ( RefType (..),
  )
where

import Data.Morpheus.Generic.Proxy (CProxy (..))
import GHC.Generics
  ( K1 (..),
    M1 (..),
    S,
    U1 (..),
    (:*:) (..),
  )
import Relude hiding (undefined)

class RefType con (f :: Type -> Type) where
  refType :: Proxy f -> Maybe (CProxy con)

instance RefType con (f :*: g) where
  refType _ = Nothing

instance (con a) => RefType con (M1 S s (K1 i a)) where
  refType _ = Just $ CProxy $ Proxy @a

instance RefType gql U1 where
  refType _ = Nothing