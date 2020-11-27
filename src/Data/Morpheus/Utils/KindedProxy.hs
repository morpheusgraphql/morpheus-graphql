{-# LANGUAGE PolyKinds #-}

module Data.Morpheus.Utils.KindedProxy
  ( KindedProxy (..),
    setType,
    setKind,
  )
where

-- | context , like Proxy with multiple parameters
-- * 'kind': object, scalar, enum ...
-- * 'a': actual gql type
data KindedProxy k a
  = KindedProxy

setType :: f a -> kinded (k :: t) a' -> KindedProxy k a
setType _ _ = KindedProxy

setKind :: f k -> kinded (k' :: t) a -> KindedProxy k a
setKind _ _ = KindedProxy
