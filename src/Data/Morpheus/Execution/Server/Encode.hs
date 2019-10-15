{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Execution.Server.Encode
  ( EncodeCon
  , EncodeMutCon
  , EncodeSubCon
  , GResolver(..)
  , Encode(..)
  , encodeQuery
  , encodeOperation
  , ObjectResolvers(..)
  , OBJ_RES
  ) where

import           Control.Monad                                   ((>=>))
import           Control.Monad.Except                            (ExceptT (..), runExceptT, withExceptT)
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
import           Data.Morpheus.Error.Selection                   (resolverError, subfieldsNotSelected)
import           Data.Morpheus.Execution.Server.Decode           (DecodeObject, decodeArguments)
import           Data.Morpheus.Execution.Server.Generics.EnumRep (EnumRep (..))
import           Data.Morpheus.Kind                              (Context (..), ENUM, GQL_KIND, OBJECT, SCALAR, UNION,
                                                                  VContext (..))
import           Data.Morpheus.Types.Custom                      (MapKind, Pair (..), mapKindFromList)
import           Data.Morpheus.Types.GQLScalar                   (GQLScalar (..))
import           Data.Morpheus.Types.GQLType                     (GQLType (CUSTOM, KIND, __typeName))
import           Data.Morpheus.Types.Internal.AST.Operation      (Operation (..), ValidOperation, getOperationName)
import           Data.Morpheus.Types.Internal.AST.Selection      (Selection (..), SelectionRec (..), SelectionSet)
import           Data.Morpheus.Types.Internal.Base               (Key)
import           Data.Morpheus.Types.Internal.Data               (MUTATION, QUERY, SUBSCRIPTION)
import           Data.Morpheus.Types.Internal.Resolver           (GADTResolver (..), GraphQLT (..), MutResolver,
                                                                  PackT (..), ResolveT, Resolver, SubResolver)
import           Data.Morpheus.Types.Internal.Stream             (Channel (..), PublishStream, SubscribeStream,
                                                                  initExceptStream, injectEvents)
import           Data.Morpheus.Types.Internal.Validation         (GQLErrors)
import           Data.Morpheus.Types.Internal.Value              (GQLValue (..), Value (..))

class Encode resolver o m e where
  encode :: resolver -> (Key, Selection) -> GraphQLT o m e Value

instance {-# OVERLAPPABLE #-} EncodeKind (KIND a) a o m e => Encode a o m e where
  encode resolver = encodeKind (VContext resolver :: VContext (KIND a) a)

-- MAYBE
instance (Monad m , Encode a o m e) => Encode (Maybe a) o m e where
  encode Nothing      = const gqlNull
  encode (Just value) = encode value

--  Tuple  (a,b)
instance Encode (Pair k v) o m e => Encode (k, v) o m e where
  encode (key, value) = encode (Pair key value)

--  Set
instance Encode [a] o m e => Encode (Set a) o m e where
  encode = encode . S.toList

--  Map
instance (Eq k, Monad m, Encode (MapKind k v (GraphQLT o m e)) o m e) => Encode (Map k v)  o m e  where
  encode value = encode ((mapKindFromList $ M.toList value) :: MapKind k v (GraphQLT o m e))

-- LIST []
instance (Monad m, Encode a o m e) => Encode [a] o m e where
  encode list query = gqlList <$> traverse (`encode` query) list

--  GQL a -> b
instance (DecodeObject a, PackT o m e ,Monad m, Encode b o m e ) => Encode (a -> b) o m e where
  encode resolver selection = decodeArgs selection >>= (`encode` selection) . resolver
    where
      decodeArgs :: (Key, Selection) -> GraphQLT o m e a
      decodeArgs = packT . decodeArguments . selectionArguments . snd

-- GQL Either Resolver Monad
--instance (Monad m, Encode a (GraphQLT o m e value)) => Encode (Either String a) (GraphQLT o m e value) where
--  encode resolver = (`encodeResolver` liftEither resolver)

--  GQL ExceptT Resolver Monad -- (GraphQLT 'Query m e value)
instance (Monad m, Encode b QUERY m e) => Encode (GADTResolver QUERY m e b) QUERY m e where
  encode (QueryResolver resolver) selection = QueryT $ encodeResolver selection resolver

--- TODO: delete me
--instance (Monad m, Encode b (ResolveT m value)) => Encode (GADTResolver 'Query m e b)  (ResolveT m value) where--
--  encode (QueryResolver resolver) = (flip encodeResolver) resolver

-- GQL Mutation Resolver Monad
instance (Monad m, Encode b MUTATION m e) => Encode (MutResolver m e b) MUTATION m e where
  encode (MutationResolver channels resolver) = MutationT . injectEvents channels . (flip encodeResolver ((ExceptT $ fmap Right  resolver):: Resolver m b))

-- GQL Subscription Resolver Monad
instance (Monad m, Encode b SUBSCRIPTION m e ) => Encode (SubResolver m event b) SUBSCRIPTION m e where
  encode resolver selection =  handleResolver resolver
    where
      handleResolver (SubscriptionResolver subChannels subResolver) =
        SubscriptionT $ initExceptStream [map Channel subChannels] ((encodeResolver selection . subResolver) :: event -> ResolveT m Value)
      --handleResolver (FailedResolving  errorMessage) = TODO: handle error

-- ENCODE GQL KIND
class EncodeKind (kind :: GQL_KIND) a o m e  where
  encodeKind :: VContext kind a -> (Key, Selection) -> GraphQLT o m e Value

-- SCALAR
instance (GQLScalar a, Monad m) => EncodeKind SCALAR a o m e where
  encodeKind = pure . gqlScalar . serialize . unVContext

-- ENUM
instance (Generic a, EnumRep (Rep a), Monad m) => EncodeKind ENUM a o m e where
  encodeKind = pure . gqlString . encodeRep . from . unVContext

--  OBJECT
instance (Monad m, EncodeCon m a o e, Monad m) => EncodeKind OBJECT a o m e where
  encodeKind (VContext value) (_, Selection {selectionRec = SelectionSet selection}) =
    resolveFields selection (__typenameResolver : objectResolvers (Proxy :: Proxy (CUSTOM a)) value)
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

type EncodeOperator m a value = Resolver m a -> ValidOperation -> m (Either GQLErrors value)

type OBJ_RES m a o e = ObjectResolvers (CUSTOM a) a o m e

type EncodeCon m a o e = (GQL_RES a, OBJ_RES m a o e)

type EncodeMutCon m event mut e = EncodeCon m mut MUTATION e

type EncodeSubCon m event sub e = EncodeCon m sub SUBSCRIPTION e

type FieldRes  o m e value  = (Key, (Key, Selection) -> GraphQLT o m e value)

type family GRes (kind :: GQL_KIND) value :: *

type instance GRes OBJECT v = [(Key, (Key, Selection) -> v)]

type instance GRes UNION v = (Key, (Key, Selection) -> v)

--- GENERICS ------------------------------------------------
class ObjectResolvers (custom :: Bool) a o m e where
  objectResolvers :: Proxy custom -> a -> [(Key, (Key, Selection) -> GraphQLT o m e Value)]

instance (Generic a, GResolver OBJECT (Rep a) o m e ) => ObjectResolvers 'False a o m e where
  objectResolvers _ = getResolvers (Context :: Context OBJECT value) . from

unionResolver :: (Generic a, GResolver UNION (Rep a) o m e) => a -> (Key, (Key, Selection) -> GraphQLT o m e Value)
unionResolver = getResolvers (Context :: Context UNION value) . from

-- | Derives resolvers for OBJECT and UNION
class GResolver (kind :: GQL_KIND) f o m e where
  getResolvers :: Context kind value -> f a -> GRes kind (GraphQLT o m e Value)

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
     forall m a schema. (GQL_RES a, GQL_RES schema, Monad m, EncodeCon m schema QUERY Value, EncodeCon m a QUERY Value)
  => schema
  -> EncodeOperator m a Value
encodeQuery schema = encodeOperationWith (objectResolvers (Proxy :: Proxy (CUSTOM schema)) schema)

encodeOperation :: (Monad m, GQL_RES a, EncodeCon m a o e, GQLValue value) => EncodeOperator m a value
encodeOperation = encodeOperationWith []

encodeOperationWith ::
     forall m a o e value. (Monad m, GQL_RES a,  EncodeCon m a o e)
  => [FieldRes o m e value]
  -> EncodeOperator m a value
encodeOperationWith externalRes rootResolver Operation {operationSelection, operationPosition, operationName} =
  runExceptT $
  operationResolveT >>=
  resolveFields operationSelection . (++) externalRes . objectResolvers (Proxy :: Proxy (CUSTOM a))
  where
    operationResolveT = withExceptT (resolverError operationPosition (getOperationName operationName)) rootResolver

encodeResolver :: (Monad m, Encode a o m e) => (Key, Selection) -> Resolver m a -> ResolveT m res
encodeResolver selection@(fieldName, Selection {selectionPosition}) =
  withExceptT (resolverError selectionPosition fieldName) >=> (`encode` selection)

resolveFields :: (Monad m, GQLValue a) => SelectionSet -> [FieldRes o m e value] -> GraphQLT o m e a
resolveFields selectionSet resolvers = gqlObject <$> traverse selectResolver selectionSet
  where
    selectResolver (key, selection) =
      (key, ) <$>
      case selectionRec selection of
        SelectionAlias name selectionRec -> lookupRes name (selection {selectionRec})
        _                                -> lookupRes key selection
        -------------------------------------------------------------
      where
        lookupRes resKey sel = (fromMaybe (const $ return gqlNull) $ lookup resKey resolvers) (key, sel)
