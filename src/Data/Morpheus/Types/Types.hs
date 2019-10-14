{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}

module Data.Morpheus.Types.Types
  ( GQLQueryRoot(..)
  , Variables
  , Undefined(..)
  ) where

import           Data.Map                                      (Map)
import           Data.Morpheus.Types.Internal.AST.Operation    (RawOperation)
import           Data.Morpheus.Types.Internal.AST.RawSelection (FragmentLib)
import           Data.Morpheus.Types.Internal.Base             (Key)
import           Data.Morpheus.Types.Internal.TH               (apply, liftTextMap)
import           Data.Morpheus.Types.Internal.Value            (Value)
import           Language.Haskell.TH.Syntax                    (Lift (..))

data Undefined (m :: * -> *) = Undefined

type Variables = Map Key Value

data GQLQueryRoot = GQLQueryRoot
  { fragments      :: FragmentLib
  , operation      :: RawOperation
  , inputVariables :: [(Key, Value)]
  } deriving (Show)

instance Lift GQLQueryRoot where
  lift (GQLQueryRoot f o v) = apply 'GQLQueryRoot [liftTextMap f, lift o, liftTextMap v]
