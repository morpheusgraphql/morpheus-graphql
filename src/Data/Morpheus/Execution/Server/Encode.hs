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
  , encodeOperation
  , ObjectResolvers(..)
  ) where

import           Data.Map                                        (Map)
import qualified Data.Map                                        as M (toList)
import           Data.Maybe                                      (fromMaybe)
import           Data.Proxy                                      (Proxy (..))
import           Data.Set                                        (Set)
import qualified Data.Set                                        as S (toList)
import           Data.Text                                       (pack)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Internal                    (internalUnknownTypeMessage)
import           Data.Morpheus.Error.Selection                   (subfieldsNotSelected)
import           Data.Morpheus.Execution.Server.Decode           (DecodeObject, decodeArguments)
import           Data.Morpheus.Execution.Server.Generics.EnumRep (EnumRep (..))
import           Data.Morpheus.Kind                              (ENUM, GQL_KIND, OBJECT, ResContext (..), SCALAR,
                                                                  UNION, VContext (..))
import           Data.Morpheus.Types.Custom                      (MapKind, Pair (..), mapKindFromList)
import           Data.Morpheus.Types.GQLScalar                   (GQLScalar (..))
import           Data.Morpheus.Types.GQLType                     (GQLType (CUSTOM, KIND, __typeName))
import           Data.Morpheus.Types.Internal.AST.Operation      (Operation (..), ValidOperation, getOperationName)
import           Data.Morpheus.Types.Internal.AST.Selection      (Selection (..), SelectionRec (..))
import           Data.Morpheus.Types.Internal.Base               (Key)
import           Data.Morpheus.Types.Internal.Data               (OperationKind, QUERY)
import           Data.Morpheus.Types.Internal.Resolver           (GADTResolver (..), GraphQLT (..), MapGraphQLT (..),
                                                                  PureOperation (..), Resolving (..), resolveObject)
import           Data.Morpheus.Types.Internal.Validation         (Validation)
import           Data.Morpheus.Types.Internal.Value              (GQLValue (..), Value (..))

data ExploreProxy (o::OperationKind) (m :: * -> *) e = ExploreProxy

class Encode resolver o m e where
  encode :: PureOperation o => resolver -> (Key, Selection) -> GraphQLT o m e Value
  exploreChannels :: ExploreProxy o m e -> resolver -> [e]

instance {-# OVERLAPPABLE #-} (EncodeKind (KIND a) a o m e , PureOperation o) => Encode a o m e where
  encode resolver = encodeKind (VContext resolver :: VContext (KIND a) a)

-- MAYBE
instance (Monad m , Encode a o m e) => Encode (Maybe a) o m e where
  encode = maybe (const $ pure gqlNull) encode
  ----------------------------------------------------------
  exploreChannels proxy = maybe []  (exploreChannels proxy)

--  Tuple  (a,b)
instance Encode (Pair k v) o m e => Encode (k, v) o m e where
  encode (key, value) = encode (Pair key value)
  ----------------------------------------------------------
  exploreChannels proxy (key,value)= exploreChannels proxy (Pair key value)

--  Set
instance Encode [a] o m e => Encode (Set a) o m e where
  encode = encode . S.toList
  ----------------------------------------------------------
  exploreChannels proxy = exploreChannels proxy . S.toList

--  Map
instance (Eq k, Monad m, Encode (MapKind k v (GADTResolver o m e)) o m e) => Encode (Map k v)  o m e  where
  encode value = encode ((mapKindFromList $ M.toList value) :: MapKind k v (GADTResolver o m e))
  -----------------------------------------------------------------------------------------
  -- TODO: exploreChannels proxy =

-- LIST []
instance (Monad m, Encode a o m e) => Encode [a] o m e where
  encode list query = gqlList <$> traverse (`encode` query) list
  exploreChannels proxy  = concatMap (exploreChannels proxy)


--  GQL a -> Resolver b, MUTATION, SUBSCRIPTION, QUERY
instance (DecodeObject a, Resolving fO m e ,Monad m,PureOperation fO, MapGraphQLT fO o, Encode b fO m e ) => Encode (a -> GADTResolver fO m e b) o m e where
  encode resolver selection@(_, Selection { selectionArguments }) = mapGraphQLT $ resolving encode (getArgs args resolver)  selection
     where
      args :: Validation a
      args =  decodeArguments selectionArguments

-- ENCODE GQL KIND
class EncodeKind (kind :: GQL_KIND) a o m e  where
  encodeKind :: PureOperation o =>  VContext kind a -> (Key, Selection) -> GraphQLT o m e Value

-- SCALAR
instance (GQLScalar a, Monad m) => EncodeKind SCALAR a o m e where
  encodeKind = pure . pure . gqlScalar . serialize . unVContext

-- ENUM
instance (Generic a, EnumRep (Rep a), Monad m) => EncodeKind ENUM a o m e where
  encodeKind = pure . pure . gqlString . encodeRep . from . unVContext

--  OBJECT
instance (Monad m, EncodeCon o m e a, Monad m) => EncodeKind OBJECT a o m e where
  encodeKind (VContext value) (_, Selection {selectionRec = SelectionSet selection}) =
    resolveObject selection (__typenameResolver : objectResolvers (Proxy :: Proxy (CUSTOM a)) value)
    where
      __typenameResolver = ("__typename", const $ pure $ gqlString $ __typeName (Proxy @a))
  encodeKind _ (key, Selection {selectionPosition}) = FailT $ subfieldsNotSelected key "" selectionPosition

-- UNION
instance (Monad m, GQL_RES a, GResolver UNION (Rep a) o m e) => EncodeKind UNION a o m e where
  encodeKind (VContext value) (key, sel@Selection {selectionRec = UnionSelection selections}) =
    resolver (key, sel {selectionRec = SelectionSet lookupSelection})
      -- SPEC: if there is no any fragment that supports current object Type GQL returns {}
    where
      lookupSelection = fromMaybe [] $ lookup typeName selections
      (typeName, resolver) = unionResolver value
  encodeKind _ _ = FailT $ internalUnknownTypeMessage "union Resolver only should recieve UnionSelection"

-- Types & Constrains -------------------------------------------------------
type GQL_RES a = (Generic a, GQLType a)

type EncodeOperator o m e a  = GADTResolver o m e a -> ValidOperation -> GraphQLT o m e Value

type EncodeCon o m e a = (GQL_RES a,  PureOperation o ,ObjectResolvers (CUSTOM a) a o m e)

type FieldRes  o m e   = (Key, (Key, Selection) -> GraphQLT o m e Value)

type family GRes (kind :: GQL_KIND) value :: *

type instance GRes OBJECT v = [(Key, (Key, Selection) -> v)]

type instance GRes UNION v = (Key, (Key, Selection) -> v)

--- GENERICS ------------------------------------------------
class ObjectResolvers (custom :: Bool) a (o :: OperationKind) (m :: * -> *) e where
  objectResolvers :: PureOperation o =>  Proxy custom -> a -> [(Key, (Key, Selection) -> GraphQLT o m e Value)]

instance (Generic a, GResolver OBJECT (Rep a) o m e ) => ObjectResolvers 'False a o m e where
  objectResolvers _ = getResolvers (ResContext :: ResContext OBJECT o m e value) . from

unionResolver :: (Generic a, PureOperation o, GResolver UNION (Rep a) o m e) => a -> (Key, (Key, Selection) -> GraphQLT o m e Value)
unionResolver = getResolvers (ResContext :: ResContext UNION o m e value) . from

-- | Derives resolvers for OBJECT and UNION
class GResolver (kind :: GQL_KIND) f o m e where
  getResolvers :: PureOperation o => ResContext kind o m e value -> f a -> GRes kind (GraphQLT o m e Value)

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
  getResolvers context (a :*: b) = getResolvers context a ++ getResolvers context b

-- UNION
instance (Selector s, GQLType a, Encode a o m e ) => GResolver UNION (M1 S s (K1 s2 a)) o m e where
  getResolvers _ (M1 (K1 src)) = (__typeName (Proxy @a), encode src)

instance (GResolver UNION a o m e, GResolver UNION b o m e) => GResolver UNION (a :+: b) o m e where
  getResolvers context (L1 x) = getResolvers context x
  getResolvers context (R1 x) = getResolvers context x

----- HELPERS ----------------------------
encodeQuery ::
     forall m event query schema. (Monad m, EncodeCon QUERY m event schema, EncodeCon QUERY m event query, Resolving QUERY m event)
  => schema
  -> EncodeOperator QUERY m event query
encodeQuery schema = encodeOperationWith (objectResolvers (Proxy :: Proxy (CUSTOM schema)) schema)

encodeOperation :: (Monad m, GQL_RES a, EncodeCon opKind m e a , Resolving opKind  m e) => EncodeOperator opKind m e a
encodeOperation = encodeOperationWith []

encodeOperationWith ::
     forall o m e a . (Monad m, EncodeCon o m e a, Resolving o m e)
  => [FieldRes o m e]
  -> EncodeOperator o m e a
encodeOperationWith externalRes rootResolver Operation {operationSelection, operationPosition, operationName} =
  gqlObject <$> resolvingObject toResolvers rootResolver (getOperationName operationName, Selection { selectionRec = SelectionSet operationSelection , selectionPosition = operationPosition })
  where
    toResolvers = objectResolvers (Proxy :: Proxy (CUSTOM a))
