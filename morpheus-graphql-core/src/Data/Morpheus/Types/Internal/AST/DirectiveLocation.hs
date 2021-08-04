{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.DirectiveLocation
  ( DirectiveLocation (..),
  )
where

import Data.Morpheus.Types.Internal.AST.Error (Msg (..))
import Language.Haskell.TH.Syntax (Lift)
import Relude hiding (Show, show)
import Prelude (Show (..))

data DirectiveLocation
  = QUERY
  | MUTATION
  | SUBSCRIPTION
  | FIELD
  | FRAGMENT_DEFINITION
  | FRAGMENT_SPREAD
  | INLINE_FRAGMENT
  | SCHEMA
  | SCALAR
  | OBJECT
  | FIELD_DEFINITION
  | ARGUMENT_DEFINITION
  | INTERFACE
  | UNION
  | ENUM
  | ENUM_VALUE
  | INPUT_OBJECT
  | INPUT_FIELD_DEFINITION
  deriving (Show, Eq, Lift)

instance Msg DirectiveLocation where
  msg = msg . show
