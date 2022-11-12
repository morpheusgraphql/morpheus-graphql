{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.Kinded
  ( KindedProxy (..),
    setType,
    setKind,
    kinded,
    CatType (..),
    inputType,
    outputType,
    CatContext (..),
    getCat,
    typeCat,
    unliftKind,
    catMap,
    addContext,
    getCatContext,
    mkScalar,
  )
where

import Data.Morpheus.Types.Internal.AST
  ( IN,
    OUT,
    ScalarDefinition,
    TRUE,
    TypeCategory (..),
    TypeContent (..),
  )
import Prelude (Show)

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

data CatContext (cat :: TypeCategory) where
  InputContext :: CatContext IN
  OutputContext :: CatContext OUT

data CatType (cat :: TypeCategory) a where
  InputType :: CatType IN a
  OutputType :: CatType OUT a

-- liftKind :: CatType cat a -> CatType cat (f k a)
-- liftKind InputType = InputType
-- liftKind OutputType = OutputType

deriving instance Show (CatType cat a)

-- converts:
--   f a -> KindedType IN a
-- or
--  f k a -> KindedType IN a
inputType :: f a -> CatType IN a
inputType _ = InputType

outputType :: f a -> CatType OUT a
outputType _ = OutputType

unliftKind :: CatType cat (f k a) -> CatType cat a
unliftKind InputType = InputType
unliftKind OutputType = OutputType

catMap :: f a -> CatType cat b -> CatType cat a
catMap _ InputType = InputType
catMap _ OutputType = OutputType

getCat :: CatContext c -> TypeCategory
getCat InputContext = IN
getCat OutputContext = OUT

typeCat :: CatType c a -> TypeCategory
typeCat InputType = IN
typeCat OutputType = OUT

addContext :: CatContext c -> f a -> CatType c a
addContext InputContext _ = InputType
addContext OutputContext _ = OutputType

getCatContext :: CatType c a -> CatContext c
getCatContext InputType = InputContext
getCatContext OutputType = OutputContext

mkScalar :: CatType c a -> ScalarDefinition -> TypeContent TRUE c s
mkScalar InputType f = DataScalar f
mkScalar OutputType f = DataScalar f
