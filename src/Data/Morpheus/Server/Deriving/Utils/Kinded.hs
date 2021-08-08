{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.Kinded
  ( KindedProxy (..),
    setType,
    setKind,
    kinded,
    KindedType (..),
    inputType,
    outputType,
    CategoryValue (..),
  )
where

import Data.Morpheus.Types.Internal.AST
  ( IN,
    LEAF,
    OUT,
    TypeCategory (..),
  )
import Prelude (Show)

class CategoryValue (c :: TypeCategory) where
  categoryValue :: f c -> TypeCategory

instance CategoryValue OUT where
  categoryValue _ = OUT

instance CategoryValue IN where
  categoryValue _ = IN

instance CategoryValue LEAF where
  categoryValue _ = LEAF

-- | context , like Proxy with multiple parameters
-- * 'kind': object, scalar, enum ...
-- * 'a': actual gql type
data KindedProxy k a
  = KindedProxy

setType :: f a -> kinded (k :: t) a' -> KindedProxy k a
setType _ _ = KindedProxy

setKind :: f k -> kinded (k' :: t) a -> KindedProxy k a
setKind _ _ = KindedProxy

kinded :: f k -> f' a -> KindedProxy k a
kinded _ _ = KindedProxy

data KindedType (cat :: TypeCategory) a where
  InputType :: KindedType IN a
  OutputType :: KindedType OUT a

deriving instance Show (KindedType cat a)

-- converts:
--   f a -> KindedType IN a
-- or
--  f k a -> KindedType IN a
inputType :: f a -> KindedType IN a
inputType _ = InputType

outputType :: f a -> KindedType OUT a
outputType _ = OutputType
