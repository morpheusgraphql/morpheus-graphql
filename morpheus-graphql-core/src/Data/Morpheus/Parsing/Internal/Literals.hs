{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Internal.Literals
  ( Lit (..),
  )
where

import GHC.TypeLits (Symbol)
import Relude hiding (ByteString, empty, many)

class Lit (l :: Symbol) where
  lit :: f l -> Word8

instance Lit "," where
  lit _ = 44
  {-# INLINEABLE lit #-}

instance Lit "_" where
  lit _ = 95
  {-# INLINEABLE lit #-}

instance Lit "!" where
  lit _ = 33
  {-# INLINEABLE lit #-}

instance Lit "$" where
  lit _ = 36
  {-# INLINEABLE lit #-}

instance Lit "@" where
  lit _ = 64
  {-# INLINEABLE lit #-}
