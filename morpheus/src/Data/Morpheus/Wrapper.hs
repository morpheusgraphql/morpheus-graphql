{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Morpheus.Wrapper
  ( GQLRoot(..)
  , EnumOf
  , ScalarOf
  , (::->)(..)
  , wrap
  , unwrap
  ) where

import           Data.Morpheus.Types.Describer ((::->) (Resolver), EnumOf (..), ScalarOf (..))
import           Data.Morpheus.Types.Types     (GQLRoot (..))

class Wrapper m where
  wrap :: a -> m a
  unwrap :: m a -> a

instance Wrapper EnumOf where
  wrap = EnumOf
  unwrap (EnumOf x) = x

instance Wrapper ScalarOf where
  wrap = ScalarOf
  unwrap (ScalarOf x) = x
