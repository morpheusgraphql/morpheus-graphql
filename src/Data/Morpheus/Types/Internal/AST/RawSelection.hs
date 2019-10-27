{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Types.Internal.AST.RawSelection
  ( Reference(..)
  , Argument(..)
  , RawArgument(..)
  , RawSelection(..)
  , Fragment(..)
  , FragmentLib
  , RawArguments
  , RawSelectionSet
  , Selection(..)
  ) where

import           Language.Haskell.TH.Syntax                 (Lift (..))

-- MORPHEUS
import           Data.Morpheus.Types.Internal.AST.Selection (Argument (..), Selection (..))
import           Data.Morpheus.Types.Internal.Base          (Collection, Key, Position, Reference (..))
import           Data.Morpheus.Types.Internal.TH            (apply, liftMaybeText, liftText, liftTextMap)

data Fragment = Fragment
  { fragmentType      :: Key
  , fragmentPosition  :: Position
  , fragmentSelection :: RawSelectionSet
  } deriving (Show)

instance Lift Fragment where
  lift (Fragment t p sel) = apply 'Fragment [liftText t, lift p, liftTextMap sel]

type RawSelection' a = Selection RawArguments a

instance Lift a => Lift (RawSelection' a) where
  lift (Selection t p alias sel) = apply 'Selection [liftTextMap t, lift p, liftMaybeText alias , lift sel]

type FragmentLib = [(Key, Fragment)]

data RawArgument
  = VariableReference Reference
  | RawArgument Argument
  deriving (Show, Lift)

type RawArguments = Collection RawArgument

type RawSelectionSet = Collection RawSelection

instance Lift RawSelection where
  lift (RawSelectionSet (Selection t p alias sel)) =
    apply 'RawSelectionSet [apply 'Selection [liftTextMap t, lift p, liftMaybeText alias,liftTextMap sel]]
  lift (RawSelectionField p) = apply 'RawSelectionField [lift p]
  lift (InlineFragment f) = apply 'InlineFragment [lift f]
  lift (Spread f) = apply 'Spread [lift f]

data RawSelection
  = RawSelectionSet (RawSelection' RawSelectionSet)
  | RawSelectionField (RawSelection' ())
  | InlineFragment Fragment
  | Spread Reference
  deriving (Show)
