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
  , GResolver(..)
  , Encode(..)
  , encodeQuery
  , encodeSubscription
  , encodeMutation
  , ObjectResolvers(..)
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
                                                ( DecodeObject
                                                , decodeArguments
                                                )
import           Data.Morpheus.Execution.Server.Generics.EnumRep
                                                ( EnumRep(..) )
import           Data.Morpheus.Kind             ( ENUM
                                                , GQL_KIND
                                                , OBJECT
                                                , ResContext(..)
                                                , SCALAR
                                                , UNION
                                                , AUTO
                                                , VContext(..)
                                                )
import           Data.Morpheus.Types.Types      ( MapKind
                                                , Pair(..)
                                                , mapKindFromList
                                                )
import           Data.Morpheus.Types.GQLScalar  ( GQLScalar(..) )
import           Data.Morpheus.Types.GQLType    ( GQLType
                                                  ( CUSTOM
                                                  , KIND
                                                  , __typeName
                                                  )
                                                )
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
instance (DecodeObject a, Monad m,LiftEither fo Resolver, MapStrategy fo o, Encode b fo e m) => Encode (a -> Resolver fo e m b) o e m where
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

--  OBJECT
instance (Monad m, EncodeCon o e m a, Monad m, GResolver OBJECT (Rep a) o e m) => EncodeKind OBJECT a o e m where
  encodeKind (VContext value) = withObject encodeK
   where
    encodeK selection = resolveObject
      selection
      (__typenameResolver : objectResolvers (Proxy :: Proxy (CUSTOM a)) value)
    __typenameResolver =
      ("__typename", const $ pure $ gqlString $ __typeName (Proxy @a))

-- exploreKindChannels
-- UNION
instance (Monad m, GQL_RES a, GResolver UNION (Rep a) o e m) => EncodeKind UNION a o e m where
  encodeKind (VContext value) (key, sel@Selection { selectionRec = UnionSelection selections })
    = resolver (key, sel { selectionRec = SelectionSet lookupSelection })
   where
    lookupSelection      = fromMaybe [] $ lookup typeName selections
    (typeName, resolver) = unionResolver value
  encodeKind _ _ = failure $ internalResolvingError "union Resolver should only recieve UnionSelection"

-- Types & Constrains -------------------------------------------------------
type GQL_RES a = (Generic a, GQLType a)

type EncodeOperator o e m a
  = a -> ValidOperation -> ResolvingStrategy o e m Value

type EncodeCon o e m a = (GQL_RES a, ObjectResolvers (CUSTOM a) a o e m)

type FieldRes o e m
  = (Key, (Key, ValidSelection) -> ResolvingStrategy o e m Value)

type family GRes (kind :: GQL_KIND) value :: *

type instance GRes OBJECT v = [(Key, (Key, ValidSelection) -> v)]

type instance GRes UNION v = (Key, (Key, ValidSelection) -> v)

--- GENERICS ------------------------------------------------
class ObjectResolvers (custom :: Bool) a (o :: OperationType) e (m :: * -> *) where
  objectResolvers :: Proxy custom -> a -> [(Key, (Key, ValidSelection) -> ResolvingStrategy o e m Value)]

instance (Generic a, GResolver OBJECT (Rep a) o e m ) => ObjectResolvers 'False a o e m where
  objectResolvers _ =
    getResolvers (ResContext :: ResContext OBJECT o e m value) . from

unionResolver
  :: (Generic a, GResolver UNION (Rep a) o e m)
  => a
  -> (Key, (Key, ValidSelection) -> ResolvingStrategy o e m Value)
unionResolver =
  getResolvers (ResContext :: ResContext UNION o e m value) . from

-- | Derives resolvers for OBJECT and UNION
class GResolver (kind :: GQL_KIND) f o e (m :: * -> *) where
  getResolvers :: ResContext kind o e m value -> f a -> GRes kind (ResolvingStrategy o e m Value)

instance GResolver kind f o e m => GResolver kind (M1 D c f) o e m where
  getResolvers context (M1 src) = getResolvers context src

instance GResolver kind f o e m => GResolver kind (M1 C c f) o e m where
  getResolvers context (M1 src) = getResolvers context src

-- OBJECT
instance GResolver OBJECT U1 o e m where
  getResolvers _ _ = []

instance (Selector s, GQLType a, Encode a o e m) => GResolver OBJECT (M1 S s (K1 s2 a)) o e m where
  getResolvers _ m@(M1 (K1 src)) = [(pack (selName m), encode src)]

instance (GResolver OBJECT f o e m, GResolver OBJECT g o e m) => GResolver OBJECT (f :*: g) o e m where
  getResolvers context (a :*: b) =
    getResolvers context a ++ getResolvers context b

-- UNION
instance (Selector s, GQLType a, Encode a o e m ) => GResolver UNION (M1 S s (K1 s2 a)) o e m where
  getResolvers _ (M1 (K1 src)) = (__typeName (Proxy @a), encode src)

instance (GResolver UNION a o e m, GResolver UNION b o e m) => GResolver UNION (a :+: b) o e m where
  getResolvers context (L1 x) = getResolvers context x
  getResolvers context (R1 x) = getResolvers context x

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

__typenameResolverBy :: (Monad m,LiftEither o ResolvingStrategy) => Name -> (Key,(Key, ValidSelection) -> ResolvingStrategy o e m Value)
__typenameResolverBy name = ("__typename", const $ pure $ gqlString name)    

-- NEW AUTOMATIC DERIVATION SYSTEM

instance (Monad m,Generic a, GQLType a,TypeRep (Rep a) o e m) => EncodeKind AUTO a o e m where
  encodeKind (VContext value) = case rawRes of
    TypeRes { resKind = REP_OBJECT, resFields } ->
      withObject (encodeK resFields)
    TypeRes { resKind = REP_UNION, resFields, resCons } -> encodeUnion
      resFields
     where
      encodeUnion [] (_, Selection { selectionRec = SelectionField }) = pure $ gqlString resCons
      encodeUnion [] (_,Selection { selectionRec = UnionSelection selections }) 
        = resolveObject currentSelection resolvers
        where 
          enumObjectTypeName = __typeName (Proxy @a) <> "EnumObject"
          currentSelection = fromMaybe [] $ lookup enumObjectTypeName selections
          resolvers = [
              ("enum",const $ pure $ gqlString resCons),
              __typenameResolverBy enumObjectTypeName
            ]
      encodeUnion [ResField { resFieldType, resFieldRes }] (key, sel@Selection { selectionRec = UnionSelection selections })
        = resFieldRes (key, sel { selectionRec = SelectionSet lookupSelection })
        where lookupSelection = fromMaybe [] $ lookup resFieldType selections
      ------------------------------------------------------------------------------
      encodeUnion resFields (key, Selection { selectionRec = UnionSelection selections })
        = resolveObject currentSelection resolvers
          where
            currentSelection = fromMaybe [] $ lookup resCons selections
            resolvers = [
                ("enum",const $ pure $ gqlString resCons),
                __typenameResolverBy resCons
              ]
      ------------------------------------------------------------------------------  
      encodeUnion _ _ = failure $ internalResolvingError "union Resolver should only recieve UnionSelection"
   where
    rawRes =
      typeResolvers (ResContext :: ResContext AUTO o e m value) (from value)
    ---------------------------------------------------------------  
    encodeK resolvers selection =
      resolveObject selection (__typenameResolver : map toObjRes resolvers)
    toObjRes ResField { resFieldName, resFieldRes } =
      (resFieldName, resFieldRes)
    __typenameResolver = __typenameResolverBy $ __typeName (Proxy @a)

data REP_KIND = REP_UNION | REP_OBJECT

data TypeRes o e m = TypeRes {
      resCons :: Name,
      resKind :: REP_KIND,
      resFields :: [ResField o e m]
  }

data ResField o e m = ResField {
    resFieldType :: Name,
    resFieldName :: Name,
    resFieldRes :: (Key, ValidSelection) -> ResolvingStrategy o e m Value
  }

class TypeRep f o e (m :: * -> *) where
          typeResolvers :: ResContext AUTO o e m value -> f a -> TypeRes o e m

instance TypeRep  f o e m => TypeRep (M1 D c f) o e m where
  typeResolvers context (M1 src) = typeResolvers context src

      --- UNION OR OBJECT 
instance (TypeRep a o e m,TypeRep b o e m) => TypeRep (a :+: b) o e m where
  typeResolvers context (L1 x) =
    (typeResolvers context x) { resKind = REP_UNION }
  typeResolvers context (R1 x) =
    (typeResolvers context x) { resKind = REP_UNION }

instance (FieldRep f o e m,Constructor c) => TypeRep (M1 C c f) o e m where
  typeResolvers context (M1 src) = TypeRes { resCons
                                           , resKind   = REP_OBJECT
                                           , resFields = fieldRep context src
                                           }
    where resCons = pack $ conName (undefined :: (M1 C c U1 x))


      --- FIELDS      
class FieldRep f o e (m :: * -> *) where
        fieldRep :: ResContext AUTO o e m value -> f a -> [ResField o e m]

instance (FieldRep f o e m, FieldRep g o e m) => FieldRep  (f :*: g) o e m where
  fieldRep context (a :*: b) = fieldRep context a <> fieldRep context b

instance (Selector s, GQLType a, Encode a o e m) => FieldRep (M1 S s (K1 s2 a)) o e m where
  fieldRep _ m@(M1 (K1 src)) =
    [ ResField { resFieldName = pack (selName m)
               , resFieldType = __typeName (Proxy @a)
               , resFieldRes  = encode src
               }
    ]

instance FieldRep U1 o e m where
  fieldRep _ _ = []

