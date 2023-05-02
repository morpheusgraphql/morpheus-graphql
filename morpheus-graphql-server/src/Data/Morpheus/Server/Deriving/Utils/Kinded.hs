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
    CatType (..),
    inputType,
    outputType,
    CatContext (..),
    unliftKind,
    catMap,
    addContext,
    getCatContext,
    mkScalar,
    isIN,
    ContextValue (..),
  )
where

import Data.Morpheus.Server.Types.Kind (DerivingKind)
import Data.Morpheus.Types.Internal.AST
  ( IN,
    OUT,
    ScalarDefinition,
    TRUE,
    TypeCategory (..),
    TypeContent (..),
  )
import Data.Proxy (Proxy (..))
import Prelude (Bool (..), Show)

-- | context , like Proxy with multiple parameters
-- * 'kind': object, scalar, enum ...
-- * 'a': actual gql type
data KindedProxy k a
  = KindedProxy

setType :: f a -> kinded (k :: t) a' -> KindedProxy k a
setType _ _ = KindedProxy

data CatContext (cat :: TypeCategory) where
  InputContext :: CatContext IN
  OutputContext :: CatContext OUT

data CatType (cat :: TypeCategory) a where
  InputType :: CatType IN a
  OutputType :: CatType OUT a

deriving instance Show (CatType cat a)

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

addContext :: CatContext c -> f a -> CatType c a
addContext InputContext _ = InputType
addContext OutputContext _ = OutputType

getCatContext :: CatType c a -> CatContext c
getCatContext InputType = InputContext
getCatContext OutputType = OutputContext

mkScalar :: CatType c a -> ScalarDefinition -> TypeContent TRUE c s
mkScalar InputType f = DataScalar f
mkScalar OutputType f = DataScalar f

isIN :: CatType c a -> Bool
isIN InputType = True
isIN _ = False

newtype ContextValue (kind :: DerivingKind) a = ContextValue
  { unContextValue :: a
  }
