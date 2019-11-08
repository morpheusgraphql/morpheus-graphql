{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE DeriveLift       #-}

module Data.Morpheus.Types.Types
  ( GQLQueryRoot(..)
  , Variables
  , Undefined(..)
  )
where

import           Data.Map                       ( Map )
import           GHC.Generics                   ( Generic )
import           Language.Haskell.TH.Syntax     ( Lift )
import           Instances.TH.Lift              ( )

-- Morpheus
import           Data.Morpheus.Types.Internal.AST.Operation
                                                ( RawOperation )
import           Data.Morpheus.Types.Internal.AST.RawSelection
                                                ( FragmentLib )
import           Data.Morpheus.Types.Internal.Base
                                                ( Key )
import           Data.Morpheus.Types.Internal.Value
                                                ( Value )

data Undefined (m :: * -> *) = Undefined deriving (Show, Generic)

type Variables = Map Key Value

data GQLQueryRoot = GQLQueryRoot
  { fragments      :: FragmentLib
  , operation      :: RawOperation
  , inputVariables :: [(Key, Value)]
  } deriving (Show,Lift)
