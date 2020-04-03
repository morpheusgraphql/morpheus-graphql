{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Error.NameCollision
  ( NameCollision(..)
  , Unknown(..)
  , KindViolation(..)
  )
where

import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Name 
                                                , GQLError(..)
                                                , GQLErrors
                                                )

class NameCollision a where 
  nameCollision :: Name -> a -> GQLError

class Unknown c where
  type UnknownSelector c
  unknown :: c -> UnknownSelector c -> GQLErrors

class KindViolation a where
  kindViolation :: a -> GQLError