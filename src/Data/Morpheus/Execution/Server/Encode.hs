{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE GADTs                 #-}

module Data.Morpheus.Execution.Server.Encode
  ( EncodeCon
  , Encode(..)
  , ExploreResolvers(..)
  , deriveRoot
  , DeriveRoot(..)
  , deriveModel
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
                                                ( toList )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Semigroup                 ( (<>) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
                                                ( toList )
import           Data.Text                      ( pack )
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Execution.Server.Decode
                                                ( DecodeType
                                                , decodeArguments
                                                )
import           Data.Morpheus.Kind             ( ENUM
                                                , GQL_KIND
                                                , ResContext(..)
                                                , SCALAR
                                                , OUTPUT
                                                , VContext(..)
                                                )
import           Data.Morpheus.Types.Types      ( MapKind
                                                , Pair(..)
                                                , mapKindFromList
                                                )
import           Data.Morpheus.Types.GQLScalar  ( GQLScalar(..) )
import           Data.Morpheus.Types.GQLType    ( GQLType(..) )
import           Data.Morpheus.Types.Internal.AST
                                                ( Name
                                                , OperationType(..)
                                                , QUERY
                                                , GQLValue(..)
                                                , ValidValue
                                                , SUBSCRIPTION
                                                , MUTATION
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( MapStrategy(..)
                                                , LiftOperation
                                                , Resolver
                                                , unsafeBind
                                                , toResolver
                                                , DataResolver(..)
                                                , resolve__typename
                                                , FieldRes
                                                , runDataResolver
                                                , GQLRootResolver(..)
                                                , ResolverModel(..)
                                                )

class Encode resolver o e (m :: * -> *) where
  encode :: resolver -> Resolver o e m ValidValue

instance {-# OVERLAPPABLE #-} (EncodeKind (KIND a) a o e m , LiftOperation o) => Encode a o e m where
  encode resolver = encodeKind (VContext resolver :: VContext (KIND a) a)

-- MAYBE
instance (Monad m , LiftOperation o,Encode a o e m) => Encode (Maybe a) o e m where
  encode = maybe (pure gqlNull) encode

-- LIST []
instance (Monad m, Encode a o e m, LiftOperation o) => Encode [a] o e m where
  encode = fmap gqlList . traverse encode

--  Tuple  (a,b)
instance Encode (Pair k v) o e m => Encode (k, v) o e m where
  encode (key, value) = encode (Pair key value)

--  Set
instance Encode [a] o e m => Encode (Set a) o e m where
  encode = encode . S.toList

--  Map
instance (Eq k, Monad m,LiftOperation o, Encode (MapKind k v (Resolver o e m)) o e m) => Encode (Map k v)  o e m where
  encode value =
    encode ((mapKindFromList $ M.toList value) :: MapKind k v (Resolver o e m))

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (DecodeType a,Generic a, Monad m,LiftOperation fo, MapStrategy fo o, Encode b fo e m) => Encode (a -> Resolver fo e m b) o e m where
 encode resolver = mapStrategy $ toResolver decodeArguments resolver `unsafeBind` encode 

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (Monad m,LiftOperation fo, MapStrategy fo o, Encode b fo e m) => Encode (Resolver fo e m b) o e m where
  encode = mapStrategy . (`unsafeBind` encode)

-- ENCODE GQL KIND
class EncodeKind (kind :: GQL_KIND) a o e (m :: * -> *) where
  encodeKind :: LiftOperation o =>  VContext kind a -> Resolver o e m ValidValue

-- SCALAR
instance (GQLScalar a, Monad m) => EncodeKind SCALAR a o e m where
  encodeKind = pure . gqlScalar . serialize . unVContext

-- ENUM
instance (Generic a,GQLType a, ExploreResolvers (CUSTOM a) a o e m, Monad m) => EncodeKind ENUM a o e m where
  encodeKind (VContext value) = runDataResolver (__typeName (Proxy @a)) (exploreResolvers (Proxy @(CUSTOM a)) value)

instance (Monad m,Generic a, GQLType a, ExploreResolvers (CUSTOM a) a o e m) => EncodeKind OUTPUT a o e m where
  encodeKind (VContext value) = runDataResolver (__typeName (Proxy @a)) (exploreResolvers (Proxy @(CUSTOM a)) value)

convertNode
  :: (Monad m, LiftOperation o)
  => ResNode o e m
  -> DataResolver o e m
convertNode ResNode { resKind = REP_OBJECT, resFields } =
  ObjectRes $ map toFieldRes resFields
convertNode ResNode { resDatatypeName, resKind = REP_UNION, resFields, resTypeName, isResRecord }
  = encodeUnion resFields
 where
  -- ENUM
  encodeUnion [] = EnumRes resTypeName
  -- Type References --------------------------------------------------------------
  encodeUnion [FieldNode { fieldTypeName, fieldResolver, isFieldObject }]
    | isFieldObject && resTypeName == resDatatypeName <> fieldTypeName
    = UnionRef (fieldTypeName, fieldResolver)
  -- RECORDS ----------------------------------------------------------------------------
  encodeUnion fields = UnionRes
    (resTypeName, resolve__typename resTypeName : map toFieldRes resolvers)
   where
    resolvers | isResRecord = fields
              | otherwise   = setFieldNames fields

-- Types & Constrains -------------------------------------------------------
type GQL_RES a = (Generic a, GQLType a)

type EncodeCon o e m a = (GQL_RES a, ExploreResolvers (CUSTOM a) a o e m)

--- GENERICS ------------------------------------------------
class ExploreResolvers (custom :: Bool) a (o :: OperationType) e (m :: * -> *) where
  exploreResolvers :: Proxy custom -> a -> DataResolver o e m

instance (Generic a,Monad m,LiftOperation o,TypeRep (Rep a) o e m ) => ExploreResolvers 'False a o e m where
  exploreResolvers _ value = convertNode
    $ typeResolvers (ResContext :: ResContext OUTPUT o e m value) (from value)

----- HELPERS ----------------------------
objectResolvers
  :: forall a o e m
   . ExploreResolvers (CUSTOM a) a o e m
  => a
  -> DataResolver o e m
objectResolvers value = case exploreResolvers (Proxy @(CUSTOM a)) value of
  ObjectRes resFields -> ObjectRes resFields
  _                   -> InvalidRes "resolver must be an object"

data DeriveRoot (o :: OperationType) e m where
   DeriveQuery 
      :: forall a e m schema. 
        ( ExploreResolvers (CUSTOM a) a QUERY e m
        , ExploreResolvers (CUSTOM schema) schema QUERY e m
        ) 
      => a 
      -> schema
      -> DeriveRoot QUERY e m
   DeriveMutation 
      :: forall a e m. 
        ( ExploreResolvers (CUSTOM a) a MUTATION e m) 
      => a 
      -> DeriveRoot MUTATION e m 
   DeriveSubscription 
      :: forall a e m. 
        ( ExploreResolvers (CUSTOM a) a SUBSCRIPTION e m) 
      => a 
      -> DeriveRoot SUBSCRIPTION e m 
    
deriveRoot 
  :: DeriveRoot op ev m 
  -> DataResolver op ev m
deriveRoot (DeriveQuery res schema)        = objectResolvers res <> objectResolvers schema
deriveRoot (DeriveMutation res)     = objectResolvers res
deriveRoot (DeriveSubscription res) = objectResolvers res


type Con o e m a 
  = ExploreResolvers 
      (CUSTOM 
        (a (Resolver o e m))
      ) 
      (a (Resolver o e m)) 
      o 
      e 
      m

deriveModel
  :: forall e m query mut sub schema. 
      ( Con QUERY e m query
      , Con QUERY e m schema
      , Con MUTATION e m mut
      , Con SUBSCRIPTION e m sub
      , Applicative m
      )
  => GQLRootResolver m e query mut sub
  -> schema (Resolver QUERY e m)
  -> ResolverModel e m
deriveModel 
  GQLRootResolver 
    { queryResolver
    , mutationResolver
    , subscriptionResolver 
    }
  schema
  = ResolverModel 
    { query = pure $ objectResolvers queryResolver <> objectResolvers schema
    , mutation = pure $ objectResolvers mutationResolver
    , subscription = pure $ objectResolvers subscriptionResolver
    }

toFieldRes :: FieldNode o e m -> FieldRes o e m
toFieldRes FieldNode { fieldSelName, fieldResolver } =
  (fieldSelName, fieldResolver)

-- NEW AUTOMATIC DERIVATION SYSTEM
data REP_KIND = REP_UNION | REP_OBJECT

data ResNode o e m = ResNode {
    resDatatypeName :: Name,
    resTypeName :: Name,
    resKind :: REP_KIND,
    resFields :: [FieldNode o e m],
    isResRecord :: Bool
  }

data FieldNode o e m = FieldNode {
    fieldTypeName :: Name,
    fieldSelName :: Name,
    fieldResolver  :: Resolver o e m ValidValue,
    isFieldObject  :: Bool
  }

-- setFieldNames ::  Power Int Text -> Power { _1 :: Int, _2 :: Text }
setFieldNames :: [FieldNode o e m] -> [FieldNode o e m]
setFieldNames = zipWith setFieldName ([0 ..] :: [Int])
  where setFieldName i field = field { fieldSelName = "_" <> pack (show i) }

class TypeRep f o e (m :: * -> *) where
  typeResolvers :: ResContext OUTPUT o e m value -> f a -> ResNode o e m

instance (Datatype d,TypeRep  f o e m) => TypeRep (M1 D d f) o e m where
  typeResolvers context (M1 src) = (typeResolvers context src)
    { resDatatypeName = pack $ datatypeName (undefined :: M1 D d f a)
    }


      --- UNION OR OBJECT 
instance (TypeRep a o e m,TypeRep b o e m) => TypeRep (a :+: b) o e m where
  typeResolvers context (L1 x) =
    (typeResolvers context x) { resKind = REP_UNION }
  typeResolvers context (R1 x) =
    (typeResolvers context x) { resKind = REP_UNION }

instance (FieldRep f o e m,Constructor c) => TypeRep (M1 C c f) o e m where
  typeResolvers context (M1 src) = ResNode { resDatatypeName = ""
                                           , resTypeName = pack (conName proxy)
                                           , resKind = REP_OBJECT
                                           , resFields = fieldRep context src
                                           , isResRecord = conIsRecord proxy
                                           }
    where proxy = undefined :: (M1 C c U1 x)

      --- FIELDS      
class FieldRep f o e (m :: * -> *) where
        fieldRep :: ResContext OUTPUT o e m value -> f a -> [FieldNode o e m]

instance (FieldRep f o e m, FieldRep g o e m) => FieldRep  (f :*: g) o e m where
  fieldRep context (a :*: b) = fieldRep context a <> fieldRep context b

instance (Selector s, GQLType a, Encode a o e m) => FieldRep (M1 S s (K1 s2 a)) o e m where
  fieldRep _ m@(M1 (K1 src)) =
    [ FieldNode { fieldSelName  = pack (selName m)
                , fieldTypeName = __typeName (Proxy @a)
                , fieldResolver = encode src
                , isFieldObject = isObjectKind (Proxy @a)
                }
    ]

instance FieldRep U1 o e m where
  fieldRep _ _ = []