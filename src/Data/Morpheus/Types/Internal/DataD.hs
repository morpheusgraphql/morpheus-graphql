{-# LANGUAGE DeriveLift #-}

module Data.Morpheus.Types.Internal.DataD
  ( TypeD(..)
  , ConsD(..)
  , QueryD(..)
  , GQLTypeD(..)
  )
where

import           Language.Haskell.TH.Syntax     ( Lift(..) )

--
-- MORPHEUS
import           Data.Morpheus.Types.Internal.Data
                                                ( DataField
                                                , DataTypeKind
                                                , Meta
                                                )

data QueryD = QueryD
  { queryText     :: String
  , queryTypes    :: [GQLTypeD]
  , queryArgsType :: Maybe TypeD
  } deriving (Show, Lift)

data GQLTypeD = GQLTypeD
  { typeD     :: TypeD
  , typeKindD :: DataTypeKind
  , typeArgD  :: [TypeD]
  } deriving (Show, Lift)

data TypeD = TypeD
  { tName      :: String
  , tNamespace :: [String]
  , tCons      :: [ConsD]
  , tMeta      :: Maybe Meta
  } deriving (Show, Lift)

data ConsD = ConsD
  { cName   :: String
  , cFields :: [DataField]
  } deriving (Show, Lift)
