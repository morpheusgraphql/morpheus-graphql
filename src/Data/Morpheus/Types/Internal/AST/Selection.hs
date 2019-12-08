{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}


module Data.Morpheus.Types.Internal.AST.Selection
  ( Argument(..)
  , Arguments
  , SelectionSet
  , SelectionRec(..)
  , ValueOrigin(..)
  , ValidSelection
  , Selection(..)
  , RawSelection
  , FragmentLib
  , RawArguments
  , RawSelectionSet
  , Fragment(..)
  , RawArgument
  , ValidSelectionSet
  , ValidArgument
  , ValidArguments
  )
where

import           Language.Haskell.TH.Syntax     ( Lift(..) )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Collection
                                                , Key
                                                , Position
                                                , Ref(..)
                                                )

import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value )

type RawSelection = Selection RAW
type ValidSelection = Selection VALID

type SelectionSet a = Collection (Selection a)
type RawSelectionSet = Collection RawSelection
type ValidSelectionSet = Collection ValidSelection


type UnionSelection = Collection (SelectionSet 'Valid)

data Process = Raw | Valid
  deriving (Lift)

type VALID = 'Valid
type RAW = 'Raw

data ValueOrigin
  = VARIABLE
  | INLINE
  deriving (Show, Lift)

data Fragment = Fragment
  { fragmentType      :: Key
  , fragmentPosition  :: Position
  , fragmentSelection :: RawSelectionSet
  } deriving (Show)

type FragmentLib = [(Key, Fragment)]


data Argument (a :: Process) where
  VariableRef ::Ref -> Argument RAW
  Argument ::{
    argumentValue    :: Value
  , argumentOrigin   :: ValueOrigin
  , argumentPosition :: Position
  } -> Argument a

instance Lift ValidArgument where
  lift (Argument v o p) = [| Argument v o p |]

type RawArgument = Argument RAW
type ValidArgument = Argument VALID

type Arguments a = Collection (Argument a)

type RawArguments = Collection RawArgument
type ValidArguments = Collection ValidArgument

instance Show (Argument a) where

data Selection (process:: Process) where
  Selection ::{
    selectionArguments :: Argument process
  , selectionPosition  :: Position
  , selectionAlias     :: Maybe Key
  , selectionRec       :: SelectionRec process
  } -> Selection process
  InlineFragment ::Fragment -> Selection RAW
  Spread ::Ref -> Selection RAW

instance Show (Selection a) where

instance Lift ValidSelection where
  lift (Selection args pos alias cont) = [| Selection args pos alias cont |]

data SelectionRec (a :: Process) where
  SelectionField ::SelectionRec a
  SelectionSet   ::SelectionSet a -> SelectionRec a
  UnionSelection ::UnionSelection -> SelectionRec VALID

instance Show (SelectionRec a) where

instance Lift (SelectionRec VALID) where
  lift (SelectionSet   s) = [| SelectionSet s |]
  lift (UnionSelection s) = [| UnionSelection s |]
  lift SelectionField     = [| SelectionField |]
