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
    CatType (..),
    inputType,
    outputType,
    unliftKind,
    mapCat,
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
import Prelude (Bool (..), Show)

-- | context , like Proxy with multiple parameters
-- * 'kind': object, scalar, enum ...
-- * 'a': actual gql type
data KindedProxy k a
  = KindedProxy

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

mapCat :: f a -> CatType cat b -> CatType cat a
mapCat _ InputType = InputType
mapCat _ OutputType = OutputType

mkScalar :: CatType c a -> ScalarDefinition -> TypeContent TRUE c s
mkScalar InputType f = DataScalar f
mkScalar OutputType f = DataScalar f

isIN :: CatType c a -> Bool
isIN InputType = True
isIN _ = False

newtype ContextValue (kind :: DerivingKind) a = ContextValue
  { unContextValue :: a
  }
