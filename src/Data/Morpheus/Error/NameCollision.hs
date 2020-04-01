{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Error.NameCollision
  ( NameCollision(..)
  , Unknown(..)
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