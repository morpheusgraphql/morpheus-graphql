{-# LANGUAGE ConstraintKinds       #-}
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

module Data.Morpheus.Resolve.Encode
  ( ObjectFieldResolvers(..)
  , EncodeCon
  , EncodeSubCon
  , Encode
  , encodeStreamRes
  , resolveBySelection
  , encodeSubStreamRes
  , resolveSubscriptionSelection
  , resolversBy
  ) where

import           Control.Monad.Trans                        (lift)
import           Control.Monad.Trans.Except
import           Data.Map                                   (Map)
import qualified Data.Map                                   as M (toList)
import           Data.Maybe                                 (fromMaybe)
import           Data.Proxy                                 (Proxy (..))
import           Data.Set                                   (Set)
import qualified Data.Set                                   as S (toList)
import           Data.Text                                  (Text, pack)
import           Data.Typeable                              (Typeable)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Internal               (internalErrorT)
import           Data.Morpheus.Error.Selection              (fieldNotResolved, subfieldsNotSelected)
import           Data.Morpheus.Kind                         (ENUM, KIND, OBJECT, SCALAR, UNION, WRAPPER)
import           Data.Morpheus.Resolve.Decode               (ArgumentsConstraint, decodeArguments)
import           Data.Morpheus.Resolve.Generics.EnumRep     (EnumRep (..))
import           Data.Morpheus.Types.Custom                 (MapKind, Pair (..), mapKindFromList)
import           Data.Morpheus.Types.GQLScalar              (GQLScalar (..))
import           Data.Morpheus.Types.GQLType                (GQLType (__typeName))
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..), SelectionSet)
import           Data.Morpheus.Types.Internal.Base          (Position)
import           Data.Morpheus.Types.Internal.Stream        (EventContent, StreamState (..), StreamT (..),
                                                             SubscribeStream)
import           Data.Morpheus.Types.Internal.Validation    (GQLErrors, ResolveT, failResolveT)
import           Data.Morpheus.Types.Internal.Value         (ScalarValue (..), Value (..))
import           Data.Morpheus.Types.Resolver               (Resolver, SubRes)

type EncodeCon m a = (Generic a, Typeable a, ObjectFieldResolvers (Rep a) (ResolveT m Value))

type Encode m a = ResolveT m a -> SelectionSet -> m (Either GQLErrors Value)

type SubT m event = ResolveT (SubscribeStream m event) (EventContent event -> ResolveT m Value)

type EncodeSubCon m event a = (Generic a, Typeable a, ObjectFieldResolvers (Rep a) (SubT m event))

encodeStreamRes :: (Monad m, EncodeCon m a) => Encode m a
encodeStreamRes rootResolver sel = runExceptT $ rootResolver >>= resolveBySelection sel . resolversBy

encodeSubStreamRes ::
     (Monad m, EncodeSubCon m event a)
  => ResolveT (SubscribeStream m event) a
  -> SelectionSet
  -> (SubscribeStream m event) (Either GQLErrors (EventContent event -> ResolveT m Value))
encodeSubStreamRes rootResolver sel = runExceptT $ rootResolver >>= resolveSubscriptionSelection sel . resolversBy

-- EXPORT -------------------------------------------------------
type ResolveSel result = [(Text, Selection)] -> [(Text, (Text, Selection) -> result)] -> result

resolveBySelection :: Monad m => ResolveSel (ResolveT m Value)
resolveBySelection selection resolvers = Object <$> mapM (selectResolver Null resolvers) selection

resolveSubscriptionSelection :: Monad m => ResolveSel (SubT m s)
resolveSubscriptionSelection selection resolvers =
  toObj <$> mapM (selectResolver (const $ pure Null) resolvers) selection
  where
    toObj pairs args = Object <$> mapM (\(key, valFunc) -> (key, ) <$> valFunc args) pairs

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

resolversBy :: (Generic a, ObjectFieldResolvers (Rep a) result) => a -> [(Text, (Text, Selection) -> result)]
resolversBy = objectFieldResolvers "" . from

---------------------------------------------------------
--
--  OBJECT
-- | Derives resolvers by object fields
class ObjectFieldResolvers f o where
  objectFieldResolvers :: Text -> f a -> [(Text, (Text, Selection) -> o)]

instance ObjectFieldResolvers U1 res where
  objectFieldResolvers _ _ = []

instance (Selector s, ObjectFieldResolvers f res) => ObjectFieldResolvers (M1 S s f) res where
  objectFieldResolvers _ m@(M1 src) = objectFieldResolvers (pack $ selName m) src

instance ObjectFieldResolvers f res => ObjectFieldResolvers (M1 D c f) res where
  objectFieldResolvers key' (M1 src) = objectFieldResolvers key' src

instance ObjectFieldResolvers f res => ObjectFieldResolvers (M1 C c f) res where
  objectFieldResolvers key' (M1 src) = objectFieldResolvers key' src

instance (ObjectFieldResolvers f res, ObjectFieldResolvers g res) => ObjectFieldResolvers (f :*: g) res where
  objectFieldResolvers meta (a :*: b) = objectFieldResolvers meta a ++ objectFieldResolvers meta b

--
-- UNION
--
class UnionResolvers f result where
  unionResolvers :: f a -> (Text, (Text, Selection) -> result)

instance UnionResolvers f res => UnionResolvers (M1 S s f) res where
  unionResolvers (M1 x) = unionResolvers x

instance UnionResolvers f res => UnionResolvers (M1 D c f) res where
  unionResolvers (M1 x) = unionResolvers x

instance UnionResolvers f res => UnionResolvers (M1 C c f) res where
  unionResolvers (M1 x) = unionResolvers x

instance (UnionResolvers a res, UnionResolvers b res) => UnionResolvers (a :+: b) res where
  unionResolvers (L1 x) = unionResolvers x
  unionResolvers (R1 x) = unionResolvers x

type ObjectConstraint a m = (Monad m, Generic a, GQLType a, ObjectFieldResolvers (Rep a) (ResolveT m Value))

type UnionConstraint a m = (Monad m, Generic a, GQLType a, UnionResolvers (Rep a) (ResolveT m Value))

type EnumConstraint a = (Generic a, EnumRep (Rep a))

newtype WithGQLKind a b = WithGQLKind
  { resolverValue :: a
  }

type GQLKindOf a = WithGQLKind a (KIND a)

encode ::
     forall a result. Encoder a (KIND a) result
  => a
  -> (Text, Selection)
  -> result
encode resolver = __encode (WithGQLKind resolver :: GQLKindOf a)

class Encoder a kind result where
  __encode :: WithGQLKind a kind -> (Text, Selection) -> result

type ResValue m = (ResolveT m Value)

--
-- SCALAR
--
instance (GQLScalar a, Monad m) => Encoder a SCALAR (ResValue m) where
  __encode = pure . pure . Scalar . serialize . resolverValue

--
-- ENUM
--
instance (EnumConstraint a, Monad m) => Encoder a ENUM (ResValue m) where
  __encode = pure . pure . Scalar . String . encodeRep . from . resolverValue

--
--  OBJECTS
--
instance ObjectConstraint a m => Encoder a OBJECT (ResValue m) where
  __encode (WithGQLKind value) (_, Selection {selectionRec = SelectionSet selection'}) =
    resolveBySelection selection' (__typenameResolver : resolversBy value)
    where
      __typenameResolver = ("__typename", const $ return $ Scalar $ String $ __typeName (Proxy @a))
  __encode _ (key, Selection {selectionPosition}) = failResolveT $ subfieldsNotSelected key "" selectionPosition

instance Encoder a (KIND a) result => ObjectFieldResolvers (K1 s a) result where
  objectFieldResolvers key' (K1 src) = [(key', encode src)]

-- | Resolves and encodes UNION,
-- Handles all operators: Query, Mutation and Subscription,
instance UnionConstraint a m => Encoder a UNION (ResValue m) where
  __encode (WithGQLKind value) (key', sel@Selection {selectionRec = UnionSelection selections'}) =
    resolver (key', sel {selectionRec = SelectionSet lookupSelection})
    where
      lookupSelection :: SelectionSet
      -- SPEC: if there is no any fragment that supports current object Type GQL returns {}
      lookupSelection = fromMaybe [] $ lookup typeName selections'
      (typeName, resolver) = unionResolvers (from value)
  __encode _ _ = internalErrorT "union Resolver only should recieve UnionSelection"

instance (GQLType a, Encoder a (KIND a) result) => UnionResolvers (K1 s a) result where
  unionResolvers (K1 src) = (__typeName (Proxy @a), encode src)

--
--  RESOLVER: ::-> and ::->>
--
-- | Handles all operators: Query, Mutation and Subscription,
-- if you use it with Mutation or Subscription all effects inside will be lost
instance (ArgumentsConstraint a, Monad m, Encoder b (KIND b) (ResValue m)) =>
         Encoder (a -> Resolver m b) WRAPPER (ResValue m) where
  __encode (WithGQLKind resolver) selection'@(fieldName, Selection {selectionArguments, selectionPosition}) = do
    args <- ExceptT $ pure $ decodeArguments selectionArguments
    lift (runExceptT $ resolver args) >>= liftEither selectionPosition fieldName >>= (`encode` selection')

liftEither :: Monad m => Position -> Text -> Either String a -> ResolveT m a
liftEither position name (Left message) = failResolveT $ fieldNotResolved position name (pack message)
liftEither _ _ (Right value)            = pure value

-- packs Monad in StreamMonad
instance (Monad m, Encoder a (KIND a) (ResValue m), ArgumentsConstraint p) =>
         Encoder (p -> Either String a) WRAPPER (ResValue m) where
  __encode (WithGQLKind resolver) selection'@(fieldName, Selection {selectionArguments, selectionPosition}) =
    case decodeArguments selectionArguments of
      Left message -> failResolveT message
      Right value  -> liftEither selectionPosition fieldName (resolver value) >>= (`encode` selection')

-- packs Monad in StreamMonad
instance (ArgumentsConstraint a, Monad m, Encoder b (KIND b) (ResValue m)) =>
         Encoder (a -> Resolver m b) WRAPPER (ResValue (StreamT m c)) where
  __encode resolver selection = ExceptT $ StreamT $ StreamState [] <$> runExceptT (__encode resolver selection)

instance (ArgumentsConstraint a, Show s, Monad m, Encoder b (KIND b) (ResValue m)) =>
         Encoder (a -> SubRes m s b) WRAPPER (ResolveT (SubscribeStream m s) (EventContent s -> ResValue m)) where
  __encode (WithGQLKind resolver) selection@(fieldName, Selection {selectionArguments, selectionPosition}) =
    case decodeArguments selectionArguments of
      Left message -> failResolveT message
      Right args ->
        ExceptT $
        StreamT $ do
          let (events, res) = resolver args
          let value = liftEitherM res
          pure $ StreamState [events] (Right value)
        where liftEitherM :: (EventContent s -> Resolver m b) -> EventContent s -> ResValue m
              liftEitherM subResolver event =
                ExceptT $ do
                  result <- runExceptT (subResolver event)
                  case result of
                    Left message -> pure $ Left $ fieldNotResolved selectionPosition fieldName (pack message)
                    Right v      -> runExceptT $ encode v selection


--
-- MAYBE
--
instance (Monad m, Encoder a (KIND a) (ResValue m)) => Encoder (Maybe a) WRAPPER (ResValue m) where
  __encode (WithGQLKind Nothing)      = const $ pure Null
  __encode (WithGQLKind (Just value)) = encode value

--
-- LIST
--
instance (Monad m, Encoder a (KIND a) (ResValue m)) => Encoder [a] WRAPPER (ResValue m) where
  __encode (WithGQLKind list) query = List <$> mapM (`__encode` query) (map WithGQLKind list :: [GQLKindOf a])

--
--  Tuple
--
instance Encoder (Pair k v) OBJECT (ResValue m) => Encoder (k, v) WRAPPER (ResValue m) where
  __encode (WithGQLKind (key, value)) = encode (Pair key value)

--
--  Set
--
instance Encoder [a] WRAPPER result => Encoder (Set a) WRAPPER result where
  __encode (WithGQLKind dataSet) = encode (S.toList dataSet)

--
--  Map
--
instance (Eq k, Monad m, Encoder (MapKind k v (Resolver m)) OBJECT (ResValue m)) =>
         Encoder (Map k v) WRAPPER (ResValue m) where
  __encode (WithGQLKind value) = encode ((mapKindFromList $ M.toList value) :: MapKind k v (Resolver m))
