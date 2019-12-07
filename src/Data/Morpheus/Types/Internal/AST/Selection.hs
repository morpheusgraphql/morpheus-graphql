{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleInstances #-}



module Data.Morpheus.Types.Internal.AST.Selection
  ( Argument(..)
  , Arguments
  , SelectionSet
  , SelectionRec(..)
  , ValueOrigin(..)
  , ValidSelection
  , Selection(..)
  , RawSelection'
  , FragmentLib
  , RawArguments
  , RawSelectionSet
  , Fragment(..)
  , RawArgument(..)
  , RawSelection(..)
  )
where

import           Language.Haskell.TH.Syntax     ( Lift )


-- MORPHEUS
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Collection
                                                , Key
                                                , Position
                                                , Ref(..)
                                                )

import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value )


-- RAW SELECTION

type RawSelection = Selection 'Raw 

type Arguments = Collection Argument


type ValidSelection = Selection 'Valid

type SelectionSet a = Collection (Selection a)

type UnionSelection = Collection (SelectionSet 'Valid)

type FragmentLib = [(Key, Fragment)]

type RawArguments = Collection (GenArgument 'Raw)

type RawSelectionSet = Collection RawSelection

data ValueOrigin
  = VARIABLE
  | INLINE
  deriving (Show, Lift)

data Fragment = Fragment
  { fragmentType      :: Key
  , fragmentPosition  :: Position
  , fragmentSelection :: RawSelectionSet
  } deriving (Show)

data Argument = Argument
  { argumentValue    :: Value
  , argumentOrigin   :: ValueOrigin
  , argumentPosition :: Position
  } 
  deriving (Show, Lift)

data Process = Raw | Valid 
  deriving (Lift)

data GenArgument (a :: Process) where 
  VariableRef :: Ref -> GenArgument 'Raw
  ArgumentValue :: Argument -> GenArgument a

instance Show (GenArgument a) where 

instance Lift (GenArgument 'Valid) where 

data Selection (process:: Process) where 
  Selection :: { 
    selectionArguments :: GenArgument process
  , selectionPosition  :: Position
  , selectionAlias     :: Maybe Key
  , selectionRec       :: rec
  } -> Selection process
  InlineFragment :: Fragment -> Selection 'Raw
  Spread :: Ref -> Selection 'Raw

instance Show (Selection a) where 

instance Lift (Selection 'Valid) where 

data SelectionRec (a :: Process) where  
    SelectionSet :: SelectionSet a -> SelectionRec a
    UnionSelection :: UnionSelection -> SelectionRec 'Valid
    SelectionField :: SelectionRec a

instance Show (SelectionRec a) where 

instance Lift (SelectionRec 'Valid) where 