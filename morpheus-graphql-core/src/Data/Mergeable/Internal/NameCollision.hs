{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.Internal.NameCollision
  ( NameCollision (..),
  )
where

import Data.Mergeable.Internal.Resolution (Indexed (..))
import Data.Morpheus.Types.Internal.AST.Error
  ( ValidationError,
  )
import Relude

class NameCollision a where
  nameCollision :: a -> ValidationError

instance NameCollision a => NameCollision (Indexed k a) where
  nameCollision = nameCollision . indexedValue
