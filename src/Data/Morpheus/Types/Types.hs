{-# LANGUAGE DeriveLift #-}

module Data.Morpheus.Types.Types
  ( GQLQueryRoot(..)
  , Variables
  ) where

import           Data.Map                                      (Map)
import           Data.Morpheus.Types.Internal.AST.Operation    (RawOperation)
import           Data.Morpheus.Types.Internal.AST.RawSelection (FragmentLib)
import           Data.Morpheus.Types.Internal.Base             (Key)
import           Data.Morpheus.Types.Internal.Value            (Value)
import           Language.Haskell.TH.Syntax                    (Lift (..))

type Variables = Map Key Value

data GQLQueryRoot = GQLQueryRoot
  { fragments      :: FragmentLib
  , operation      :: RawOperation
  , inputVariables :: [(Key, Value)]
  } deriving (Show, Lift)
