{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Morpheus.Wrapper
  ( GQLRoot(..)
  , (::->)(..)
  ) where

import           Data.Morpheus.Types.Describer ((::->) (Resolver))
import           Data.Morpheus.Types.Types     (GQLRoot (..))
