{-# LANGUAGE DeriveLift       #-}

module Data.Morpheus.Types.Internal.AST
  ( GQLQuery(..)
  , Variables
  )
where

import           Data.Map                       ( Map )
import           Language.Haskell.TH.Syntax     ( Lift )
import           Instances.TH.Lift              ( )

-- Morpheus
import           Data.Morpheus.Types.Internal.AST.Operation
                                                ( RawOperation )
import           Data.Morpheus.Types.Internal.AST.Selection
                                                ( FragmentLib )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Key )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value )

type Variables = Map Key Value

data GQLQuery = GQLQuery
  { fragments      :: FragmentLib
  , operation      :: RawOperation
  , inputVariables :: [(Key, Value)]
  } deriving (Show,Lift)
