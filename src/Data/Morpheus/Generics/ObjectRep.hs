{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}

module Data.Morpheus.Generics.ObjectRep
  ( ObjectRep(..)
  , resolveTypes
  ) where

import           Data.Morpheus.Types.Internal.Data (DataTypeLib)
import           Data.Proxy                        (Proxy (..))
import           GHC.Generics

shift :: a -> (a -> b) -> b
shift x y = y x

resolveTypes :: DataTypeLib -> [DataTypeLib -> DataTypeLib] -> DataTypeLib
resolveTypes = foldl shift

class ObjectRep rep t where
  getFields :: Proxy rep -> [(t, DataTypeLib -> DataTypeLib)]

instance ObjectRep f t => ObjectRep (M1 D x f) t where
  getFields _ = getFields (Proxy @f)

instance ObjectRep f t => ObjectRep (M1 C x f) t where
  getFields _ = getFields (Proxy @f)

instance (ObjectRep a t, ObjectRep b t) => ObjectRep (a :*: b) t where
  getFields _ = getFields (Proxy @a) ++ getFields (Proxy @b)

instance ObjectRep U1 t where
  getFields _ = []
