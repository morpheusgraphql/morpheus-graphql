{-# LANGUAGE DeriveLift #-}

module Data.Morpheus.Types.Internal.DataD
  ( FieldD(..)
  , TypeD(..)
  , ConsD(..)
  , QueryD(..)
  , GQLTypeD(..)
  ) where

import           Language.Haskell.TH.Syntax        (Lift (..))

--
-- MORPHEUS
import           Data.Morpheus.Types.Internal.Data (KindD, ResolverKind, WrapperD)

data GQLTypeD = GQLTypeD
  { typeD     :: TypeD
  , typeKindD :: KindD
  , typeArgD  :: [TypeD]
  } deriving (Show, Lift)

data QueryD = QueryD
  { queryText     :: String
  , queryTypes    :: [TypeD]
  , queryArgTypes :: [TypeD]
  } deriving (Show, Lift)

data FieldD = FieldD
  { fieldNameD :: String
  , fieldArgsD :: Maybe (String, ResolverKind)
  , fieldTypeD :: ([WrapperD], (String, [String]))
  } deriving (Show, Lift)

data TypeD = TypeD
  { tName :: String
  , tCons :: [ConsD]
  } deriving (Show, Lift)

data ConsD = ConsD
  { cName   :: String
  , cFields :: [FieldD]
  } deriving (Show, Lift)
