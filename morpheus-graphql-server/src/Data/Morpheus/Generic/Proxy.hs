{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Generic.Proxy
  ( conNameProxy,
    isRecordProxy,
    symbolName,
    CProxy (..),
  )
where

import GHC.Generics
  ( C,
    Constructor,
    M1 (..),
    Meta,
    U1 (..),
    conIsRecord,
    conName,
  )
import GHC.TypeLits
import Relude hiding (undefined)
import Prelude (undefined)

conNameProxy :: forall f t (c :: Meta). (Constructor c, IsString t) => f c -> t
conNameProxy _ = fromString $ conName (undefined :: M1 C c U1 a)

isRecordProxy :: forall f (c :: Meta). (Constructor c) => f c -> Bool
isRecordProxy _ = conIsRecord (undefined :: (M1 C c f a))

symbolName :: (KnownSymbol a, IsString t) => f a -> t
symbolName = fromString . symbolVal

-- | constrained proxy
data CProxy constraint where
  CProxy :: forall f constraint a. (constraint a) => f a -> CProxy constraint