{-# LANGUAGE DeriveLift #-}

module Data.Morpheus.Types.Internal.DataD
  ( FieldD(..)
  , TypeD(..)
  , ConsD(..)
  , QueryD(..)
  , AppD(..)
  , GQLTypeD
  , gqlToHSWrappers
  ) where

import           Language.Haskell.TH.Syntax        (Lift (..))

--
-- MORPHEUS
import           Data.Morpheus.Types.Internal.Data (DataTypeKind, DataTypeWrapper (..))

data AppD a
  = ListD (AppD a)
  | MaybeD (AppD a)
  | ResD String
         String
         (AppD a)
  | BaseD a
  deriving (Show, Lift)

gqlToHSWrappers :: [DataTypeWrapper] -> a -> AppD a
gqlToHSWrappers []                             = MaybeD . BaseD
gqlToHSWrappers [NonNullType]                  = BaseD
gqlToHSWrappers (NonNullType:(ListType:xs))    = ListD . gqlToHSWrappers xs
gqlToHSWrappers (NonNullType:(NonNullType:xs)) = gqlToHSWrappers xs
gqlToHSWrappers (ListType:xs)                  = MaybeD . ListD . gqlToHSWrappers xs

type GQLTypeD = (TypeD, DataTypeKind, [TypeD])

data QueryD = QueryD
  { queryText     :: String
  , queryTypes    :: [TypeD]
  , queryArgTypes :: [TypeD]
  } deriving (Show, Lift)

data FieldD = FieldD
  { fieldNameD :: String
  , fieldTypeD :: AppD String
  } deriving (Show, Lift)


data TypeD = TypeD
  { tName :: String
  , tCons :: [ConsD]
  } deriving (Show, Lift)

data ConsD = ConsD
  { cName   :: String
  , cFields :: [FieldD]
  } deriving (Show, Lift)
