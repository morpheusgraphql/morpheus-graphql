{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
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
  , encodeMut
  , encodeSub
  ) where

import           Control.Monad                                   ((>=>))
import           Control.Monad.Trans.Except
import           Data.Map                                        (Map)
import qualified Data.Map                                        as M (toList)
import           Data.Maybe                                      (fromMaybe)
import           Data.Proxy                                      (Proxy (..))
import           Data.Set                                        (Set)
import qualified Data.Set                                        as S (toList)
import           Data.Text                                       (Text, pack)
import           Data.Typeable                                   (Typeable)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Internal                    (internalErrorT)
import           Data.Morpheus.Error.Selection                   (fieldNotResolved, subfieldsNotSelected)
import           Data.Morpheus.Execution.Server.Decode           (DecodeObject, decodeArguments)
import           Data.Morpheus.Execution.Server.Generics.EnumRep (EnumRep (..))
import           Data.Morpheus.Kind                              (ENUM, GQL_KIND, OBJECT, SCALAR, UNION, WRAPPER)
import           Data.Morpheus.Types.Custom                      (MapKind, Pair (..), mapKindFromList)
import           Data.Morpheus.Types.GQLScalar                   (GQLScalar (..))
import           Data.Morpheus.Types.GQLType                     (GQLType (KIND, __typeName))
import           Data.Morpheus.Types.Internal.AST.Operation      (Operation (..), ValidOperation)
import           Data.Morpheus.Types.Internal.AST.Selection      (Selection (..), SelectionRec (..), SelectionSet)
import           Data.Morpheus.Types.Internal.Base               (Position)
import           Data.Morpheus.Types.Internal.Stream             (PublishStream, StreamState (..), StreamT (..),
                                                                  SubscribeStream)
import           Data.Morpheus.Types.Internal.Validation         (GQLErrors, ResolveT, failResolveT)
import           Data.Morpheus.Types.Internal.Value              (ScalarValue (..), Value (..))
import           Data.Morpheus.Types.Resolver                    (Event (..), Resolver, SubResolveT, SubResolver (..))

class DefaultValue a where
  nullValue :: a
  listValue :: [a] -> a
  stringValue :: Text -> a
  objectValue :: [(Text, a)] -> a

instance DefaultValue Value where
  nullValue = Null
  listValue = List
  stringValue = Scalar . String
  objectValue = Object

class Encode a result where
  encode :: a -> (Text, Selection) -> result
  default encode :: Encoder a (KIND a) result =>
    a -> (Text, Selection) -> result
  encode resolver = __encode (WithGQLKind resolver :: GQLKindOf a)

instance {-# OVERLAPPABLE #-} Encoder a (KIND a) res => Encode a res

-- MAYBE
instance (Monad m, DefaultValue res, Encode a (m res)) => Encode (Maybe a) (m res) where
  encode Nothing      = const $ pure (nullValue :: res)
  encode (Just value) = encode value

--  Tuple  (a,b)
instance Encode (Pair k v) result => Encode (k, v) result where
  encode (key, value) = encode (Pair key value)

--  Set
instance Encode [a] result => Encode (Set a) result where
  encode = encode . S.toList

--  Map
instance (Eq k, Monad m, Encode (MapKind k v (Resolver m)) (ResolveT m res)) => Encode (Map k v) (ResolveT m res) where
  encode value = encode ((mapKindFromList $ M.toList value) :: MapKind k v (Resolver m))

-- LIST []
instance (Monad m, DefaultValue res, Encode a (m res)) => Encode [a] (m res) where
  encode list query = listValue <$> mapM (`encode` query) list

--
--  RESOLVERS
--
-- | Handles all operators: Query, Mutation and Subscription,
-- if you use it with Mutation or Subscription all effects inside will be lost
-- Pure Resolver
instance (Monad m, Encode a (ResolveT m res), DecodeObject p) => Encode (p -> Either String a) (ResolveT m res) where
  encode resolver selection = decodeArgs selection >>= encodeResolver selection . (ExceptT . pure . resolver)

--  GQL Resolver
instance (DecodeObject a, Monad m, Encode b (ResolveT m res)) => Encode (a -> Resolver m b) (ResolveT m res) where
  encode resolver selection = decodeArgs selection >>= encodeResolver selection . resolver

-- Mutation Resolver
instance (DecodeObject a, Monad m, Encode b (ResolveT m res)) =>
         Encode (a -> Resolver m b) (ResolveT (StreamT m c) res) where
  encode resolver selection = ExceptT $ StreamT $ StreamState [] <$> runExceptT (encode resolver selection)

-- Subscription Resolver
instance (DecodeObject a, Monad m, Encode b (ResolveT m Value)) =>
         Encoder (a -> SubResolver m e c b) WRAPPER (SubResolveT m e c Value) where
  __encode (WithGQLKind resolver) selection = decodeArgs selection >>= handleResolver . resolver
    where
      handleResolver SubResolver {subChannels, subResolver} =
        ExceptT $ StreamT $ pure $ StreamState [subChannels] (Right $ encodeResolver selection . subResolver)

-- EXPORT -------------------------------------------------------
type EncodeOperator m a value = Resolver m a -> ValidOperation -> m (Either GQLErrors value)

type EncodeCon m a v = (Generic a, Typeable a, GResolver (Rep a) (ResolveT m v))

type EncodeMutCon m event con mut = EncodeCon (PublishStream m event con) mut Value

type EncodeSubCon m event con sub = EncodeCon (SubscribeStream m event) sub (Event event con -> ResolveT m Value)

encodeQuery :: (Monad m, EncodeCon m schema Value, EncodeCon m a Value) => schema -> EncodeOperator m a Value
encodeQuery types rootResolver operator@Operation {operationSelection} =
  runExceptT
    (fmap resolversBy (operatorToResolveT operator rootResolver) >>=
     resolveBySelection operationSelection . (++) (resolversBy types))

encodeMut :: (Monad m, EncodeCon m a Value) => EncodeOperator m a Value
encodeMut = encodeOperator resolveBySelection

encodeSub ::
     (Monad m, EncodeSubCon m event con a)
  => EncodeOperator (SubscribeStream m event) a (Event event con -> ResolveT m Value)
encodeSub = encodeOperator (flip resolveSelection)
  where
    resolveSelection resolvers = fmap toObj . mapM (selectResolver (const $ pure nullValue) resolvers)
      where
        toObj pairs args = objectValue <$> mapM (\(key, valFunc) -> (key, ) <$> valFunc args) pairs

---------------------------------------------------------
--
--  OBJECT
-- | Derives resolvers by object fields
class GResolver f result where
  objectFieldResolvers :: f a -> [(Text, (Text, Selection) -> result)]
  unionResolvers :: f a -> (Text, (Text, Selection) -> result)

instance GResolver U1 res where
  objectFieldResolvers _ = []

instance (Selector s, GQLType a, Encode a res) => GResolver (M1 S s (K1 s2 a)) res where
  objectFieldResolvers m@(M1 (K1 src)) = [(pack $ selName m, encode src)]
  unionResolvers (M1 (K1 src)) = (__typeName (Proxy @a), encode src)

instance GResolver f res => GResolver (M1 D c f) res where
  objectFieldResolvers (M1 src) = objectFieldResolvers src
  unionResolvers (M1 x) = unionResolvers x

instance GResolver f res => GResolver (M1 C c f) res where
  objectFieldResolvers (M1 src) = objectFieldResolvers src
  unionResolvers (M1 x) = unionResolvers x

instance (GResolver f res, GResolver g res) => GResolver (f :*: g) res where
  objectFieldResolvers (a :*: b) = objectFieldResolvers a ++ objectFieldResolvers b

instance (GResolver a res, GResolver b res) => GResolver (a :+: b) res where
  unionResolvers (L1 x) = unionResolvers x
  unionResolvers (R1 x) = unionResolvers x

type ResConstraint a m res = (Monad m, Generic a, GResolver (Rep a) (ResolveT m res))

type EnumConstraint a = (Generic a, EnumRep (Rep a))

newtype WithGQLKind a (b :: GQL_KIND) = WithGQLKind
  { resolverValue :: a
  }

type GQLKindOf a = WithGQLKind a (KIND a)

class Encoder a kind result where
  __encode :: WithGQLKind a kind -> (Text, Selection) -> result

-- type ResValue m = (ResolveT m Value)
-- SCALAR
instance (GQLScalar a, Monad m) => Encoder a SCALAR (m Value) where
  __encode = pure . pure . Scalar . serialize . resolverValue

-- ENUM
instance (EnumConstraint a, Monad m) => Encoder a ENUM (m Value) where
  __encode = pure . pure . Scalar . String . encodeRep . from . resolverValue

--  OBJECTS
instance (GQLType a, DefaultValue res, ResConstraint a m res) => Encoder a OBJECT (ResolveT m res) where
  __encode (WithGQLKind value) (_, Selection {selectionRec = SelectionSet selection}) =
    resolveBySelection selection (__typenameResolver : resolversBy value)
    where
      __typenameResolver = ("__typename", const $ return $ stringValue $ __typeName (Proxy @a))
  __encode _ (key, Selection {selectionPosition}) = failResolveT $ subfieldsNotSelected key "" selectionPosition

-- | Resolves and encodes UNION,
-- Handles all operators: Query, Mutation and Subscription,
instance ResConstraint a m res => Encoder a UNION (ResolveT m res) where
  __encode (WithGQLKind value) (key', sel@Selection {selectionRec = UnionSelection selections'}) =
    resolver (key', sel {selectionRec = SelectionSet lookupSelection})
    where
      lookupSelection :: SelectionSet
      -- SPEC: if there is no any fragment that supports current object Type GQL returns {}
      lookupSelection = fromMaybe [] $ lookup typeName selections'
      (typeName, resolver) = unionResolvers (from value)
  __encode _ _ = internalErrorT "union Resolver only should recieve UnionSelection"

----- HELPERS ----------------------------
type ResolveSel result = SelectionSet -> [(Text, (Text, Selection) -> result)] -> result

resolverToResolveT :: Monad m => Position -> Text -> Resolver m a -> ResolveT m a
resolverToResolveT pos name = ExceptT . toResolveM
  where
    toResolveM :: Monad m => Resolver m a -> m (Either GQLErrors a)
    toResolveM resolver = runExceptT resolver >>= runExceptT . liftEither
      where
        liftEither :: Monad m => Either String a -> ResolveT m a
        liftEither (Left message) = failResolveT $ fieldNotResolved pos name (pack message)
        liftEither (Right value)  = pure value

encodeResolver :: (Monad m, Encode a (ResolveT m res)) => (Text, Selection) -> Resolver m a -> ResolveT m res
encodeResolver selection@(fieldName, Selection {selectionPosition}) =
  resolverToResolveT selectionPosition fieldName >=> (`encode` selection)

decodeArgs :: (Monad m, DecodeObject a) => (Text, Selection) -> ResolveT m a
decodeArgs (_, Selection {selectionArguments}) = ExceptT $ pure $ decodeArguments selectionArguments

operatorToResolveT :: Monad m => ValidOperation -> Resolver m a -> ResolveT m a
operatorToResolveT Operation {operationPosition, operationName} = resolverToResolveT operationPosition operationName

encodeOperator :: (Monad m, EncodeCon m a res) => ResolveSel (ResolveT m res) -> EncodeOperator m a res
encodeOperator resSel rootResolver operation@Operation {operationSelection} =
  runExceptT (operatorToResolveT operation rootResolver >>= resSel operationSelection . resolversBy)

resolveBySelection :: (Monad m, DefaultValue res) => ResolveSel (ResolveT m res)
resolveBySelection selection resolvers = objectValue <$> mapM (selectResolver nullValue resolvers) selection

selectResolver :: Monad m => a -> [(Text, (Text, Selection) -> m a)] -> (Text, Selection) -> m (Text, a)
selectResolver defaultValue resolvers (key, selection) =
  case selectionRec selection of
    SelectionAlias name selectionRec -> unwrapMonadTuple (key, lookupResolver name (selection {selectionRec}))
    _                                -> unwrapMonadTuple (key, lookupResolver key selection)
  where
    unwrapMonadTuple :: Monad m => (Text, m a) -> m (Text, a)
    unwrapMonadTuple (text, ioa) = ioa >>= \x -> pure (text, x)
    -------------------------------------------------------------
    lookupResolver resolverKey sel =
      (fromMaybe (const $ return $defaultValue) $ lookup resolverKey resolvers) (key, sel)

resolversBy :: (Generic a, GResolver (Rep a) result) => a -> [(Text, (Text, Selection) -> result)]
resolversBy = objectFieldResolvers . from
--------------------------------------------
