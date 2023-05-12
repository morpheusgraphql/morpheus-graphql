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
  ( conNameP,
    isRecordP,
    selNameP,
    symbolName,
    CProxy (..),
    CBox (..),
    rep,
    foldProxy,
    runCBox,
  )
where

import Data.List (init, last)
import GHC.Generics
  ( C,
    Constructor,
    Generic (..),
    M1 (..),
    Meta,
    S,
    Selector (..),
    U1 (..),
    conIsRecord,
    conName,
  )
import GHC.TypeLits
import Relude hiding (init, last, undefined)
import Prelude (undefined)

conNameP :: forall f t (c :: Meta). (Constructor c, IsString t) => f c -> t
conNameP _ = fromString $ conName (undefined :: M1 C c U1 a)

dropLiterals :: String -> String
dropLiterals name
  | not (null name) && (last name == '\'') = init name
  | otherwise = name
{-# INLINE dropLiterals #-}

isRecordP :: forall f (c :: Meta). (Constructor c) => f c -> Bool
isRecordP _ = conIsRecord (undefined :: (M1 C c f a))

selNameP :: forall f t (s :: Meta). (Selector s, IsString t) => f s -> t
selNameP _ = fromString $ dropLiterals $ selName (undefined :: M1 S s f a)

symbolName :: (KnownSymbol a, IsString t) => f a -> t
symbolName = fromString . symbolVal

-- | constrained proxy
data CProxy constraint where
  CProxy :: forall f constraint a. (constraint a) => f a -> CProxy constraint

foldProxy :: (forall f a. (c a) => f a -> b) -> CProxy c -> b
foldProxy f (CProxy x) = f x

data CBox box constraint where
  CBox :: forall constraint box a. (constraint a) => box a -> CBox box constraint

runCBox :: (forall a. (c a) => f a -> b) -> CBox f c -> b
runCBox f (CBox x) = f x

rep :: f a -> Proxy (Rep a)
rep _ = Proxy
