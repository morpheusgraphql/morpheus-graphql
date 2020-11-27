{-# LANGUAGE PolyKinds #-}

module Data.Morpheus.Utils.KindedProxy
  ( KindedProxy (..),
    setProxyType,
  )
where

-- | context , like Proxy with multiple parameters
-- * 'kind': object, scalar, enum ...
-- * 'a': actual gql type
data KindedProxy k a
  = KindedProxy

setProxyType :: f b -> kinded (k :: t) a -> KindedProxy k b
setProxyType _ _ = KindedProxy
