{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.NameCollision
  ( NameCollision (..),
  )
where

import Data.Morpheus.Types.Internal.AST.Base
  ( ValidationError,
  )

class NameCollision a where
  nameCollision :: a -> ValidationError
