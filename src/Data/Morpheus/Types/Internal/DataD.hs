module Data.Morpheus.Types.Internal.DataD
  ( TypeD(..)
  , ConsD(..)
  , QueryD(..)
  , GQLTypeD(..)
  )
where


import           Data.Morpheus.Types.Internal.Data
                                                ( DataField
                                                , DataTypeKind
                                                , Meta
                                                )

data QueryD = QueryD
  { queryText     :: String
  , queryTypes    :: [GQLTypeD]
  , queryArgsType :: Maybe TypeD
  } deriving (Show)

data GQLTypeD = GQLTypeD
  { typeD     :: TypeD
  , typeKindD :: DataTypeKind
  , typeArgD  :: [TypeD]
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
