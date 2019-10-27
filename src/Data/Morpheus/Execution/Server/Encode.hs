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
  ) where

import           Data.Map                                        (Map)
import qualified Data.Map                                        as M (toList)
import           Data.Maybe                                      (fromMaybe)
import           Data.Proxy                                      (Proxy (..))
import           Data.Semigroup                                  ((<>))
import           Data.Set                                        (Set)
import qualified Data.Set                                        as S (toList)
import           Data.Text                                       (pack)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Internal                    (internalUnknownTypeMessage)
import           Data.Morpheus.Execution.Server.Decode           (DecodeObject, decodeArguments)
import           Data.Morpheus.Execution.Server.Generics.EnumRep (EnumRep (..))
import           Data.Morpheus.Kind                              (ENUM, GQL_KIND, OBJECT, ResContext (..), SCALAR,
                                                                  UNION, VContext (..))
import           Data.Morpheus.Types.Custom                      (MapKind, Pair (..), mapKindFromList)
import           Data.Morpheus.Types.GQLScalar                   (GQLScalar (..))
import           Data.Morpheus.Types.GQLType                     (GQLType (CUSTOM, KIND, __typeName))
import           Data.Morpheus.Types.Internal.AST.Operation      (Operation (..), ValidOperation)
import           Data.Morpheus.Types.Internal.AST.Selection      (Selection (..),ValidSelection, SelectionRec (..))
import           Data.Morpheus.Types.Internal.Base               (Key)
import           Data.Morpheus.Types.Internal.Data               (MUTATION, OperationKind, QUERY, SUBSCRIPTION)
import           Data.Morpheus.Types.Internal.Resolver           (MapGraphQLT (..), PureOperation (..), Resolver (..),
                                                                  Resolving (..), ResolvingStrategy (..), resolveObject,
                                                                  withObject)
import           Data.Morpheus.Types.Internal.Validation         (Validation)
import           Data.Morpheus.Types.Internal.Value              (GQLValue (..), Value (..))

class Encode resolver o m e where
  encode :: PureOperation o => resolver -> (Key, ValidSelection) -> ResolvingStrategy o m e Value

instance {-# OVERLAPPABLE #-} (EncodeKind (KIND a) a o m e , PureOperation o) => Encode a o m e where
  encode resolver = encodeKind (VContext resolver :: VContext (KIND a) a)

-- MAYBE
instance (Monad m , Encode a o m e) => Encode (Maybe a) o m e where
  encode = maybe (const $ pure gqlNull) encode

--  Tuple  (a,b)
instance Encode (Pair k v) o m e => Encode (k, v) o m e where
  encode (key, value) = encode (Pair key value)

--  Set
instance Encode [a] o m e => Encode (Set a) o m e where
  encode = encode . S.toList

--  Map
instance (Eq k, Monad m, Encode (MapKind k v (Resolver o m e)) o m e) => Encode (Map k v)  o m e  where
  encode value = encode ((mapKindFromList $ M.toList value) :: MapKind k v (Resolver o m e))

-- LIST []
instance (Monad m, Encode a o m e) => Encode [a] o m e where
  encode list query = gqlList <$> traverse (`encode` query) list


--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (DecodeObject a, Resolving fO m e ,Monad m,PureOperation fO, MapGraphQLT fO o, Encode b fO m e ) => Encode (a -> Resolver fO m e b) o m e where
  encode resolver selection@(_, Selection { selectionArguments }) = mapGraphQLT $ resolving encode (getArgs args resolver)  selection
     where
      args :: Validation a
      args =  decodeArguments selectionArguments

-- ENCODE GQL KIND
class EncodeKind (kind :: GQL_KIND) a o m e  where
  encodeKind :: PureOperation o =>  VContext kind a -> (Key, Selection) -> ResolvingStrategy o m e Value

-- SCALAR
instance (GQLScalar a, Monad m) => EncodeKind SCALAR a o m e where
  encodeKind = pure . pure . gqlScalar . serialize . unVContext

-- ENUM
instance (Generic a, EnumRep (Rep a), Monad m) => EncodeKind ENUM a o m e where
  encodeKind = pure . pure . gqlString . encodeRep . from . unVContext

--  OBJECT
instance (Monad m, EncodeCon o m e a, Monad m, GResolver OBJECT (Rep a) o m e) => EncodeKind OBJECT a o m e where
  encodeKind (VContext value)  = withObject encodeK
    where
      encodeK selection = resolveObject selection (__typenameResolver : objectResolvers (Proxy :: Proxy (CUSTOM a)) value)
      __typenameResolver = ("__typename", const $ pure $ gqlString $ __typeName (Proxy @a))

-- exploreKindChannels
-- UNION
instance (Monad m, GQL_RES a, GResolver UNION (Rep a) o m e) => EncodeKind UNION a o m e where
  encodeKind (VContext value) (key, sel@Selection {selectionRec = UnionSelection selections}) =
    resolver (key, sel {selectionRec = SelectionSet lookupSelection})
      -- SPEC: if there is no any fragment that supports current object Type GQL returns {}
    where
      lookupSelection = fromMaybe [] $ lookup typeName selections
      (typeName, resolver) = unionResolver value
  encodeKind _ _ = Fail $ internalUnknownTypeMessage "union Resolver only should recieve UnionSelection"

-- Types & Constrains -------------------------------------------------------
type GQL_RES a = (Generic a, GQLType a)

type EncodeOperator o m e a  = a -> ValidOperation -> ResolvingStrategy o m e Value

type EncodeCon o m e a = (GQL_RES a,  ObjectResolvers (CUSTOM a) a o m e)

type FieldRes  o m e   = (Key, (Key, Selection) -> ResolvingStrategy o m e Value)

type family GRes (kind :: GQL_KIND) value :: *

type instance GRes OBJECT v = [(Key, (Key, Selection) -> v)]

type instance GRes UNION v = (Key, (Key, Selection) -> v)

--- GENERICS ------------------------------------------------
class ObjectResolvers (custom :: Bool) a (o :: OperationKind) (m :: * -> *) e where
  objectResolvers :: PureOperation o =>  Proxy custom -> a -> [(Key, (Key, Selection) -> ResolvingStrategy o m e Value)]

instance (Generic a, GResolver OBJECT (Rep a) o m e ) => ObjectResolvers 'False a o m e where
  objectResolvers _ = getResolvers (ResContext :: ResContext OBJECT o m e value) . from

unionResolver :: (Generic a, PureOperation o, GResolver UNION (Rep a) o m e) => a -> (Key, (Key, Selection) -> ResolvingStrategy o m e Value)
unionResolver = getResolvers (ResContext :: ResContext UNION o m e value) . from

-- | Derives resolvers for OBJECT and UNION
class GResolver (kind :: GQL_KIND) f o m e where
  getResolvers :: PureOperation o => ResContext kind o m e value -> f a -> GRes kind (ResolvingStrategy o m e Value)

instance GResolver kind f o m e => GResolver kind (M1 D c f) o m e where
  getResolvers context (M1 src) = getResolvers context src

instance GResolver kind f o m e => GResolver kind (M1 C c f) o m e where
  getResolvers context (M1 src) = getResolvers context src

-- OBJECT
instance GResolver OBJECT U1 o m e where
  getResolvers _ _ = []

instance (Selector s, GQLType a, Encode a o m e) => GResolver OBJECT (M1 S s (K1 s2 a)) o m e where
  getResolvers _ m@(M1 (K1 src)) = [(pack (selName m), encode src)]

instance (GResolver OBJECT f o m e, GResolver OBJECT g o m e) => GResolver OBJECT (f :*: g) o m e where
  getResolvers context (a :*: b)  = getResolvers context a  ++ getResolvers context b

-- UNION
instance (Selector s, GQLType a, Encode a o m e ) => GResolver UNION (M1 S s (K1 s2 a)) o m e where
  getResolvers _ (M1 (K1 src)) = (__typeName (Proxy @a), encode src)

instance (GResolver UNION a o m e, GResolver UNION b o m e) => GResolver UNION (a :+: b) o m e where
  getResolvers context (L1 x) = getResolvers context x
  getResolvers context (R1 x) = getResolvers context x

----- HELPERS ----------------------------
encodeQuery ::
     forall m event query (schema :: (* -> *) -> *). (Monad m, EncodeCon QUERY m event (schema (Resolver QUERY m event)), EncodeCon QUERY m event query, Resolving QUERY m event)
  => schema (Resolver QUERY m event)
  -> EncodeOperator QUERY m event query
encodeQuery schema = encodeOperationWith (objectResolvers (Proxy :: Proxy (CUSTOM (schema (Resolver QUERY m event)))) schema)

encodeMutation ::
     forall m event mut. (Monad m, EncodeCon MUTATION m event mut, Resolving MUTATION m event)
  => EncodeOperator MUTATION m event mut
encodeMutation = encodeOperationWith []

encodeSubscription ::
     forall m event mut. (Monad m, EncodeCon SUBSCRIPTION m event mut, Resolving SUBSCRIPTION m event)
  => EncodeOperator SUBSCRIPTION m event mut
encodeSubscription = encodeOperationWith []

encodeOperationWith ::
     forall o m e a . (Monad m, EncodeCon o m e a, Resolving o m e, PureOperation o)
  => [FieldRes o m e]
  -> EncodeOperator o m e a
encodeOperationWith externalRes rootResolver Operation {operationSelection} = resolveObject operationSelection resolvers
    where
       resolvers = externalRes <> objectResolvers (Proxy :: Proxy (CUSTOM a)) rootResolver
