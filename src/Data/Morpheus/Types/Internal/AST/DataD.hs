module Data.Morpheus.Types.Internal.AST.DataD
  ( TypeD(..)
  , ConsD(..)
  , ClientQuery(..)
  , GQLTypeD(..)
  , ClientType(..)
  )
where

import           Data.Morpheus.Types.Internal.AST.Data
                                                ( DataField
                                                , DataTypeKind
                                                , Meta
                                                , DataType(..)
                                                , Name
                                                )

-- CLIENT                                                
data ClientQuery = ClientQuery
  { queryText     :: String
  , queryTypes    :: [ClientType]
  , queryArgsType :: Maybe TypeD
  } deriving (Show)

data ClientType = ClientType {
  clientType :: TypeD,
  clientKind :: DataTypeKind
} deriving (Show)

-- Document
data GQLTypeD = GQLTypeD
  { typeD     :: TypeD
  , typeKindD :: DataTypeKind
  , typeArgD  :: [TypeD]
  , typeOriginal:: (Name,DataType)
  } deriving (Show)

data TypeD = TypeD
  { tName      :: String
  , tNamespace :: [String]
  , tCons      :: [ConsD]
  , tMeta      :: Maybe Meta
  } deriving (Show)

data ConsD = ConsD
  { cName   :: String
  , cFields :: [DataField]
  } deriving (Show)
