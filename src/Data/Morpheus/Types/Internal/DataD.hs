{-# LANGUAGE DeriveLift #-}

module Data.Morpheus.Types.Internal.DataD
  ( FieldD(..)
  , TypeD(..)
  , ConsD(..)
  , QueryD(..)
  , AppD(..)
  , GQLTypeD(..)
  , ResolverKind(..)
  , KindD(..)
  , unKindD
  , isObject
  , isInput
  , gqlToHSWrappers
  ) where

import           Language.Haskell.TH.Syntax        (Lift (..))

--
-- MORPHEUS
import           Data.Morpheus.Types.Internal.Data (DataTypeKind (..), DataTypeWrapper (..))

data ResolverKind
  = PlainResolver
  | TypeVarResolver
  | ExternalResolver
  deriving (Show, Eq, Lift)

data AppD a
  = ListD (AppD a)
  | MaybeD (AppD a)
  | BaseD a
  deriving (Show, Lift)

gqlToHSWrappers :: [DataTypeWrapper] -> a -> AppD a
gqlToHSWrappers []                             = MaybeD . BaseD
gqlToHSWrappers [NonNullType]                  = BaseD
gqlToHSWrappers (NonNullType:(ListType:xs))    = ListD . gqlToHSWrappers xs
gqlToHSWrappers (NonNullType:(NonNullType:xs)) = gqlToHSWrappers xs
gqlToHSWrappers (ListType:xs)                  = MaybeD . ListD . gqlToHSWrappers xs

unKindD :: KindD -> DataTypeKind
unKindD SubscriptionD       = KindObject
unKindD (RegularKindD kind) = kind

isObject :: KindD -> Bool
isObject (RegularKindD KindObject)      = True
isObject (RegularKindD KindInputObject) = True
isObject _                              = False

isInput :: KindD -> Bool
isInput (RegularKindD KindInputObject) = True
isInput _                              = False

data KindD
  = SubscriptionD
  | RegularKindD DataTypeKind
  deriving (Show, Eq, Lift)

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
  , fieldTypeD :: AppD (String, [String])
  } deriving (Show, Lift)

data TypeD = TypeD
  { tName :: String
  , tCons :: [ConsD]
  } deriving (Show, Lift)

data ConsD = ConsD
  { cName   :: String
  , cFields :: [FieldD]
  } deriving (Show, Lift)
