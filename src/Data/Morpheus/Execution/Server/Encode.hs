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

module Data.Morpheus.Execution.Server.Encode
  ( EncodeCon
  , Encode(..)
  , encodeQuery
  , encodeSubscription
  , encodeMutation
  , ResolveNode(..)
  , ResNode
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
                                                ( toList )
import           Data.Maybe                     ( fromMaybe )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Semigroup                 ( (<>) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
                                                ( toList )
import           Data.Text                      ( pack )
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Internal   ( internalResolvingError )
import           Data.Morpheus.Execution.Server.Decode
                                                ( DecodeType
                                                , decodeArguments
                                                )
import           Data.Morpheus.Execution.Server.Generics.EnumRep
                                                ( EnumRep(..) )
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
                                                , Operation(..)
                                                , ValidOperation
                                                , Key
                                                , MUTATION
                                                , OperationType
                                                , QUERY
                                                , SUBSCRIPTION
                                                , Selection(..)
                                                , SelectionRec(..)
                                                , ValidSelection
                                                , GQLValue(..)
                                                , Value(..)
                                                , SelectionSet
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( MapStrategy(..)
                                                , LiftEither(..)
                                                , Resolver(..)
                                                , resolving
                                                , toResolver
                                                , ResolvingStrategy(..)
                                                , resolveObject
                                                , withObject
                                                , Validation
                                                , failure
                                                )

class Encode resolver o e (m :: * -> *) where
  encode :: resolver -> (Key, ValidSelection) -> ResolvingStrategy o e m Value

instance {-# OVERLAPPABLE #-} (EncodeKind (KIND a) a o e m , LiftEither o ResolvingStrategy) => Encode a o e m where
  encode resolver = encodeKind (VContext resolver :: VContext (KIND a) a)

-- MAYBE
instance (Monad m , LiftEither o ResolvingStrategy,Encode a o e m) => Encode (Maybe a) o e m where
  encode = maybe (const $ pure gqlNull) encode

--  Tuple  (a,b)
instance Encode (Pair k v) o e m => Encode (k, v) o e m where
  encode (key, value) = encode (Pair key value)

--  Set
instance Encode [a] o e m => Encode (Set a) o e m where
  encode = encode . S.toList

--  Map
instance (Eq k, Monad m,LiftEither o Resolver, Encode (MapKind k v (Resolver o e m)) o e m) => Encode (Map k v)  o e m where
  encode value =
    encode ((mapKindFromList $ M.toList value) :: MapKind k v (Resolver o e m))

-- LIST []
instance (Monad m, Encode a o e m, LiftEither o ResolvingStrategy) => Encode [a] o e m where
  encode list query = gqlList <$> traverse (`encode` query) list

--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (DecodeType a,Generic a, Monad m,LiftEither fo Resolver, MapStrategy fo o, Encode b fo e m) => Encode (a -> Resolver fo e m b) o e m where
  encode resolver selection@(_, Selection { selectionArguments }) =
    mapStrategy $ resolving encode (toResolver args resolver) selection
   where
    args :: Validation a
    args = decodeArguments selectionArguments

-- ENCODE GQL KIND
class EncodeKind (kind :: GQL_KIND) a o e (m :: * -> *) where
  encodeKind :: LiftEither o ResolvingStrategy =>  VContext kind a -> (Key, ValidSelection) -> ResolvingStrategy o e m Value

-- SCALAR
instance (GQLScalar a, Monad m) => EncodeKind SCALAR a o e m where
  encodeKind = pure . pure . gqlScalar . serialize . unVContext

-- ENUM
instance (Generic a, EnumRep (Rep a), Monad m) => EncodeKind ENUM a o e m where
  encodeKind = pure . pure . gqlString . encodeRep . from . unVContext

instance (Monad m,Generic a, GQLType a,ResolveNode (CUSTOM a) a o e m) => EncodeKind OUTPUT a o e m where
  encodeKind (VContext value) = case resolveNode (Proxy @(CUSTOM a)) value of
    ResNode { resKind = REP_OBJECT, resFields } ->
      withObject (encodeK resFields)
    ResNode { resKind = REP_UNION, resFields, resTypeName, isResRecord } ->
      encodeUnion resFields
     where
        -- ENUM
      encodeUnion [] (_, Selection { selectionRec = SelectionField }) =
        pure $ gqlString resTypeName
      -- BOXED ENUM
      encodeUnion [] (_, Selection { selectionRec = UnionSelection selections })
        = resolveObject currentSelection resolvers
       where
        enumObjectTypeName = __typeName (Proxy @a) <> "EnumObject"
        currentSelection   = fromMaybe [] $ lookup enumObjectTypeName selections
        resolvers =
          [ ("enum", const $ pure $ gqlString resTypeName)
          , __typenameResolverBy enumObjectTypeName
          ]
      encodeUnion [] _ =
        failure $ internalResolvingError "expected enum value Error"
      -- Type References --------------------------------------------------------------
      encodeUnion [ResField { resFieldType, resFieldRes, resIsObject }] (key, sel@Selection { selectionRec = UnionSelection selections })
        | resIsObject && resTypeName == __typeName (Proxy @a) <> resFieldType
        = resFieldRes
          (key, sel { selectionRec = SelectionSet currentSelection })
        where currentSelection = pickSelection resFieldType selections
      -- RECORDS ----------------------------------------------------------------------------
      encodeUnion fields (_, Selection { selectionRec = UnionSelection selections })
        | isResRecord
        = resolveObject selection resolvers
       where
        selection = pickSelection resTypeName selections
        resolvers = __typenameResolverBy resTypeName : map toFieldRes fields
      --- Types without Record fields
      encodeUnion fields (_, Selection { selectionRec = UnionSelection selections })
        = resolveObject selection resolvers
       where
        selection = fromMaybe [] $ lookup resTypeName selections
        resolvers = __typenameResolverBy resTypeName
          : map toFieldRes (setFieldNames fields)
      ------------------------------------------------------------------------------  
      encodeUnion _ _ = failure $ internalResolvingError
        "union Resolver should only recieve UnionSelection"
   where


    ---------------------------------------------------------------  
    encodeK resolvers selection =
      resolveObject selection (__typenameResolver : map toFieldRes resolvers)
    __typenameResolver = __typenameResolverBy $ __typeName (Proxy @a)

-- Types & Constrains -------------------------------------------------------
type GQL_RES a = (Generic a, GQLType a)

type EncodeOperator o e m a
  = a -> ValidOperation -> ResolvingStrategy o e m Value

type EncodeCon o e m a = (GQL_RES a, ResolveNode (CUSTOM a) a o e m)

type FieldRes o e m
  = (Key, (Key, ValidSelection) -> ResolvingStrategy o e m Value)

objectResolvers
  :: forall custom a o e m
   . ResolveNode custom a o e m
  => Proxy custom
  -> a
  -> [(Key, (Key, ValidSelection) -> ResolvingStrategy o e m Value)]
objectResolvers isCustom value = case resolveNode isCustom value of
  ResNode { resKind = REP_OBJECT, resFields } -> map toFieldRes resFields
  -- TODO: FIXME:
  _ -> []


--- GENERICS ------------------------------------------------
class ResolveNode (custom :: Bool) a (o :: OperationType) e (m :: * -> *) where
  resolveNode :: Proxy custom -> a -> ResNode o e m

instance (Generic a, TypeRep (Rep a) o e m ) => ResolveNode 'False a o e m where
  resolveNode _ value =
    typeResolvers (ResContext :: ResContext OUTPUT o e m value) (from value)

----- HELPERS ----------------------------
encodeQuery
  :: forall m event query (schema :: (* -> *) -> *)
   . ( Monad m
     , EncodeCon QUERY event m (schema (Resolver QUERY event m))
     , EncodeCon QUERY event m query
     )
  => schema (Resolver QUERY event m)
  -> EncodeOperator QUERY event m query
encodeQuery schema = encodeOperationWith
  (objectResolvers (Proxy :: Proxy (CUSTOM (schema (Resolver QUERY event m))))
                   schema
  )

encodeMutation
  :: forall event m mut
   . (Monad m, EncodeCon MUTATION event m mut)
  => EncodeOperator MUTATION event m mut
encodeMutation = encodeOperationWith []

encodeSubscription
  :: forall m event mut
   . (Monad m, EncodeCon SUBSCRIPTION event m mut)
  => EncodeOperator SUBSCRIPTION event m mut
encodeSubscription = encodeOperationWith []

encodeOperationWith
  :: forall o e m a
   . (Monad m, EncodeCon o e m a, LiftEither o ResolvingStrategy)
  => [FieldRes o e m]
  -> EncodeOperator o e m a
encodeOperationWith externalRes rootResolver Operation { operationSelection } =
  resolveObject operationSelection resolvers
 where
  resolvers =
    externalRes <> objectResolvers (Proxy :: Proxy (CUSTOM a)) rootResolver

__typenameResolverBy
  :: (Monad m, LiftEither o ResolvingStrategy)
  => Name
  -> (Key, (Key, ValidSelection) -> ResolvingStrategy o e m Value)
__typenameResolverBy name = ("__typename", const $ pure $ gqlString name)

-- NEW AUTOMATIC DERIVATION SYSTEM

pickSelection :: Name -> [(Name, SelectionSet)] -> SelectionSet
pickSelection name = fromMaybe [] . lookup name

data REP_KIND = REP_UNION | REP_OBJECT

data ResNode o e m = ResNode {
    resTypeName :: Name,
    resKind :: REP_KIND,
    resFields :: [FieldNode o e m],
    isResRecord :: Bool
  }

data FieldNode o e m = FieldNode {
    fieldTypeName :: Name,
    fieldSelName :: Name,
    fieldResolver  :: (Key, ValidSelection) -> ResolvingStrategy o e m Value,
    isFieldObject  :: Bool
  }

toFieldRes :: FieldNode o e m -> FieldRes o e m
toFieldRes FieldNode { fieldSelName, fieldResolver } =
  (fieldSelName, fieldResolver)

-- setFieldNames ::  Power Int Text -> Power { _1 :: Int, _2 :: Text }
setFieldNames :: [FieldNode o e m] -> [FieldNode o e m]
setFieldNames = zipWith setFieldName ([0 ..] :: [Int])
  where setFieldName i field = field { fieldSelName = "_" <> pack (show i) }

class TypeRep f o e (m :: * -> *) where
          typeResolvers :: ResContext OUTPUT o e m value -> f a -> ResNode o e m

instance TypeRep  f o e m => TypeRep (M1 D c f) o e m where
  typeResolvers context (M1 src) = typeResolvers context src

      --- UNION OR OBJECT 
instance (TypeRep a o e m,TypeRep b o e m) => TypeRep (a :+: b) o e m where
  typeResolvers context (L1 x) =
    (typeResolvers context x) { resKind = REP_UNION }
  typeResolvers context (R1 x) =
    (typeResolvers context x) { resKind = REP_UNION }

instance (FieldRep f o e m,Constructor c) => TypeRep (M1 C c f) o e m where
  typeResolvers context (M1 src) = ResNode { resTypeName = pack (conName proxy)
                                           , resKind     = REP_OBJECT
                                           , resFields   = fieldRep context src
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

