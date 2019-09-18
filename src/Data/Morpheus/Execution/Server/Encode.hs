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
  , encodeQuery
  , encodeOperation
  ) where

import           Control.Monad                                   ((>=>))
import           Control.Monad.Except                            (liftEither, runExceptT, withExceptT)
import           Data.Map                                        (Map)
import qualified Data.Map                                        as M (toList)
import           Data.Maybe                                      (fromMaybe)
import           Data.Proxy                                      (Proxy (..))
import           Data.Set                                        (Set)
import qualified Data.Set                                        as S (toList)
import           Data.Text                                       (pack)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Internal                    (internalErrorT)
import           Data.Morpheus.Error.Selection                   (resolverError, subfieldsNotSelected)
import           Data.Morpheus.Execution.Server.Decode           (DecodeObject, decodeArguments)
import           Data.Morpheus.Execution.Server.Generics.EnumRep (EnumRep (..))
import           Data.Morpheus.Kind                              (Context (..), ENUM, GQL_KIND, OBJECT, SCALAR, UNION,
                                                                  VContext (..))
import           Data.Morpheus.Types.Custom                      (MapKind, Pair (..), mapKindFromList)
import           Data.Morpheus.Types.GQLScalar                   (GQLScalar (..))
import           Data.Morpheus.Types.GQLType                     (GQLType (KIND, __typeName))
import           Data.Morpheus.Types.Internal.AST.Operation      (Operation (..), ValidOperation)
import           Data.Morpheus.Types.Internal.AST.Selection      (Selection (..), SelectionRec (..), SelectionSet)
import           Data.Morpheus.Types.Internal.Base               (Key)
import           Data.Morpheus.Types.Internal.Stream             (PublishStream, StreamT (..), SubscribeStream,
                                                                  initExceptStream, injectEvents)
import           Data.Morpheus.Types.Internal.Validation         (GQLErrors, ResolveT, failResolveT)
import           Data.Morpheus.Types.Internal.Value              (GQLValue (..), Value (..))
import           Data.Morpheus.Types.Resolver                    (Event (..), Resolver, SubResolveT, SubResolver (..))

class Encode resolver value where
  encode :: resolver -> (Key, Selection) -> value

instance {-# OVERLAPPABLE #-} EncodeKind (KIND a) a res => Encode a res where
  encode resolver = encodeKind (VContext resolver :: VContext (KIND a) a)

-- MAYBE
instance (GQLValue value, Encode a value) => Encode (Maybe a) value where
  encode Nothing      = const gqlNull
  encode (Just value) = encode value

--  Tuple  (a,b)
instance Encode (Pair k v) value => Encode (k, v) value where
  encode (key, value) = encode (Pair key value)

--  Set
instance Encode [a] value => Encode (Set a) value where
  encode = encode . S.toList

--  Map
instance (Eq k, Monad m, Encode (MapKind k v (Resolver m)) (ResolveT m value)) =>
         Encode (Map k v) (ResolveT m value) where
  encode value = encode ((mapKindFromList $ M.toList value) :: MapKind k v (Resolver m))

-- LIST []
instance (Monad m, GQLValue value, Encode a (m value)) => Encode [a] (m value) where
  encode list query = gqlList <$> traverse (`encode` query) list

-- GQL Either Resolver
instance (Monad m, Encode a (ResolveT m value), DecodeObject p) =>
         Encode (p -> Either String a) (ResolveT m value) where
  encode resolver selection = decodeArgs selection >>= encodeResolver selection . (liftEither . resolver)

--  GQL ExceptT Resolver
instance (DecodeObject a, Monad m, Encode b (ResolveT m value)) => Encode (a -> Resolver m b) (ResolveT m value) where
  encode resolver selection = decodeArgs selection >>= encodeResolver selection . resolver

-- GQL Mutation Resolver
instance (DecodeObject a, Monad m, Encode b (ResolveT m value)) =>
         Encode (a -> Resolver m b) (ResolveT (StreamT m c) value) where
  encode resolver = injectEvents [] . encode resolver

-- GQL Subscription Resolver
instance (DecodeObject a, Monad m, Encode b (ResolveT m Value)) =>
         Encode (a -> SubResolver m e c b) (SubResolveT m e c Value) where
  encode resolver selection = decodeArgs selection >>= handleResolver . resolver
    where
      handleResolver SubResolver {subChannels, subResolver} =
        initExceptStream [subChannels] (encodeResolver selection . subResolver)

-- ENCODE GQL KIND
class EncodeKind (kind :: GQL_KIND) a value where
  encodeKind :: VContext kind a -> (Key, Selection) -> value

-- SCALAR
instance (GQLScalar a, GQLValue value) => EncodeKind SCALAR a value where
  encodeKind = pure . gqlScalar . serialize . unVContext

-- ENUM
instance (Generic a, EnumRep (Rep a), GQLValue value) => EncodeKind ENUM a value where
  encodeKind = pure . gqlString . encodeRep . from . unVContext

--  OBJECT
instance (GQL_RES m a value, GResolver OBJECT (Rep a) (ResolveT m value)) =>
         EncodeKind OBJECT a (ResolveT m value) where
  encodeKind (VContext value) (_, Selection {selectionRec = SelectionSet selection}) =
    resolveFields selection (__typenameResolver : objectResolvers value)
    where
      __typenameResolver = ("__typename", const $ pure $ gqlString $ __typeName (Proxy @a))
  encodeKind _ (key, Selection {selectionPosition}) = failResolveT $ subfieldsNotSelected key "" selectionPosition

-- UNION
instance (GQL_RES m a value, GResolver UNION (Rep a) (ResolveT m value)) => EncodeKind UNION a (ResolveT m value) where
  encodeKind (VContext value) (key, sel@Selection {selectionRec = UnionSelection selections}) =
    resolver (key, sel {selectionRec = SelectionSet lookupSelection})
      -- SPEC: if there is no any fragment that supports current object Type GQL returns {}
    where
      lookupSelection = fromMaybe [] $ lookup typeName selections
      (typeName, resolver) = unionResolver value
  encodeKind _ _ = internalErrorT "union Resolver only should recieve UnionSelection"

-- Types & Constrains -------------------------------------------------------
type GQL_RES m a value = (Monad m, Generic a, GQLType a, GQLValue value)

type EncodeOperator m a value = Resolver m a -> ValidOperation -> m (Either GQLErrors value)

type EncodeCon m a value = (Generic a, GResolver OBJECT (Rep a) (ResolveT m value))

type EncodeMutCon m event con mut = EncodeCon (PublishStream m event con) mut Value

type EncodeSubCon m event con sub = EncodeCon (SubscribeStream m event) sub (Event event con -> ResolveT m Value)

type FieldRes m value = (Key, (Key, Selection) -> ResolveT m value)

type family GRes (kind :: GQL_KIND) value :: *

type instance GRes OBJECT v = [(Key, (Key, Selection) -> v)]

type instance GRes UNION v = (Key, (Key, Selection) -> v)

--- GENERICS ------------------------------------------------
class ObjectResolvers a value where
  objectResolvers :: a -> [(Key, (Key, Selection) -> value)]

instance (Generic a, GResolver OBJECT (Rep a) value) => ObjectResolvers a value where
  objectResolvers = getResolvers (Context :: Context OBJECT value) . from

unionResolver :: (Generic a, GResolver UNION (Rep a) value) => a -> (Key, (Key, Selection) -> value)
unionResolver = getResolvers (Context :: Context UNION value) . from

-- | Derives resolvers for OBJECT and UNION
class GResolver (kind :: GQL_KIND) f value where
  getResolvers :: Context kind value -> f a -> GRes kind value

instance GResolver kind f value => GResolver kind (M1 D c f) value where
  getResolvers context (M1 src) = getResolvers context src

instance GResolver kind f value => GResolver kind (M1 C c f) value where
  getResolvers context (M1 src) = getResolvers context src

-- OBJECT
instance GResolver OBJECT U1 value where
  getResolvers _ _ = []

instance (Selector s, GQLType a, Encode a value) => GResolver OBJECT (M1 S s (K1 s2 a)) value where
  getResolvers _ m@(M1 (K1 src)) = [(pack (selName m), encode src)]

instance (GResolver OBJECT f value, GResolver OBJECT g value) => GResolver OBJECT (f :*: g) value where
  getResolvers context (a :*: b) = getResolvers context a ++ getResolvers context b

-- UNION
instance (Selector s, GQLType a, Encode a value) => GResolver UNION (M1 S s (K1 s2 a)) value where
  getResolvers _ (M1 (K1 src)) = (__typeName (Proxy @a), encode src)

instance (GResolver UNION a value, GResolver UNION b value) => GResolver UNION (a :+: b) value where
  getResolvers context (L1 x) = getResolvers context x
  getResolvers context (R1 x) = getResolvers context x

----- HELPERS ----------------------------
encodeQuery :: (Monad m, EncodeCon m schema Value, EncodeCon m a Value) => schema -> EncodeOperator m a Value
encodeQuery schema = encodeOperationWith (objectResolvers schema)

encodeOperation :: (Monad m, EncodeCon m a value, GQLValue value) => EncodeOperator m a value
encodeOperation = encodeOperationWith []

encodeOperationWith :: (Monad m, EncodeCon m a value, GQLValue value) => [FieldRes m value] -> EncodeOperator m a value
encodeOperationWith externalRes rootResolver Operation {operationSelection, operationPosition, operationName} =
  runExceptT $ operationResolveT >>= resolveFields operationSelection . (++) externalRes . objectResolvers
  where
    operationResolveT = withExceptT (resolverError operationPosition operationName) rootResolver

encodeResolver :: (Monad m, Encode a (ResolveT m res)) => (Key, Selection) -> Resolver m a -> ResolveT m res
encodeResolver selection@(fieldName, Selection {selectionPosition}) =
  withExceptT (resolverError selectionPosition fieldName) >=> (`encode` selection)

decodeArgs :: (Monad m, DecodeObject a) => (Key, Selection) -> ResolveT m a
decodeArgs = liftEither . decodeArguments . selectionArguments . snd

resolveFields :: (Monad m, GQLValue a) => SelectionSet -> [FieldRes m a] -> ResolveT m a
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
