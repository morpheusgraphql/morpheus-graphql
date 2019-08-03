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
  ( EncodeCon
  , EncodeMutCon
  , EncodeSubCon
  , encodeQuery
  , encodeMut
  , encodeSub
  ) where

import           Control.Monad                              ((>=>))
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
import           Data.Morpheus.Kind                         (ENUM, OBJECT, SCALAR, UNION, WRAPPER)
import           Data.Morpheus.Resolve.Decode               (ArgumentsConstraint, decodeArguments)
import           Data.Morpheus.Resolve.Generics.EnumRep     (EnumRep (..))
import           Data.Morpheus.Types.Custom                 (MapKind, Pair (..), mapKindFromList)
import           Data.Morpheus.Types.GQLScalar              (GQLScalar (..))
import           Data.Morpheus.Types.GQLType                (GQLType (KIND, __typeName))
import           Data.Morpheus.Types.Internal.AST.Operator  (Operator' (..), ValidOperator')
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..), SelectionSet)
import           Data.Morpheus.Types.Internal.Base          (Position)
import           Data.Morpheus.Types.Internal.Stream        (PublishStream, StreamState (..), StreamT (..),
                                                             SubscribeStream)
import           Data.Morpheus.Types.Internal.Validation    (GQLErrors, ResolveT, failResolveT)
import           Data.Morpheus.Types.Internal.Value         (ScalarValue (..), Value (..))
import           Data.Morpheus.Types.Resolver               (Event (..), Resolver, SubResolveT, SubResolver)

type EncodeOperator m a value = Resolver m a -> ValidOperator' -> m (Either GQLErrors value)

-- EXPORT -------------------------------------------------------
type EncodeCon m a v = (Generic a, Typeable a, ObjectFieldResolvers (Rep a) (ResolveT m v))

type EncodeMutCon m event con mut = EncodeCon (PublishStream m event con) mut Value

type EncodeSubCon m event con sub = EncodeCon (SubscribeStream m event) sub (con -> ResolveT m Value)

encodeQuery :: (Monad m, EncodeCon m schema Value, EncodeCon m a Value) => schema -> EncodeOperator m a Value
encodeQuery types rootResolver operator@Operator' {operatorSelection} =
  runExceptT
    (fmap resolversBy (operatorToResolveT operator rootResolver) >>=
     resolveBySelection operatorSelection . (++) (resolversBy types))

encodeMut :: (Monad m, EncodeCon m a Value) => EncodeOperator m a Value
encodeMut = encodeOperator resolveBySelection

encodeSub ::
     (Monad m, EncodeSubCon m event con a) => EncodeOperator (SubscribeStream m event) a (con -> ResolveT m Value)
encodeSub = encodeOperator (flip resolveSelection)
  where
    resolveSelection resolvers = fmap toObj . mapM (selectResolver (const $ pure Null) resolvers)
      where
        toObj pairs args = Object <$> mapM (\(key, valFunc) -> (key, ) <$> valFunc args) pairs

---------------------------------------------------------
--
--  OBJECT
-- | Derives resolvers by object fields
class ObjectFieldResolvers f o where
  objectFieldResolvers :: f a -> [(Text, (Text, Selection) -> o)]

instance ObjectFieldResolvers U1 res where
  objectFieldResolvers _ = []

instance (Selector s, Encoder a (KIND a) res) => ObjectFieldResolvers (M1 S s (K1 s2 a)) res where
  objectFieldResolvers m@(M1 (K1 src)) = [(pack $ selName m, encode src)]

instance ObjectFieldResolvers f res => ObjectFieldResolvers (M1 D c f) res where
  objectFieldResolvers (M1 src) = objectFieldResolvers src

instance ObjectFieldResolvers f res => ObjectFieldResolvers (M1 C c f) res where
  objectFieldResolvers (M1 src) = objectFieldResolvers src

instance (ObjectFieldResolvers f res, ObjectFieldResolvers g res) => ObjectFieldResolvers (f :*: g) res where
  objectFieldResolvers (a :*: b) = objectFieldResolvers a ++ objectFieldResolvers b

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
--  RESOLVERS
--
-- | Handles all operators: Query, Mutation and Subscription,
-- if you use it with Mutation or Subscription all effects inside will be lost
instance (ArgumentsConstraint a, Monad m, Encoder b (KIND b) (ResValue m)) =>
         Encoder (a -> Resolver m b) WRAPPER (ResValue m) where
  __encode (WithGQLKind resolver) selection = decodeArgs selection >>= encodeResolver selection . resolver

-- packs Monad in StreamMonad
instance (Monad m, Encoder a (KIND a) (ResValue m), ArgumentsConstraint p) =>
         Encoder (p -> Either String a) WRAPPER (ResValue m) where
  __encode (WithGQLKind resolver) selection =
    decodeArgs selection >>= encodeResolver selection . (ExceptT . pure . resolver)

-- packs Monad in StreamMonad
instance (ArgumentsConstraint a, Monad m, Encoder b (KIND b) (ResValue m)) =>
         Encoder (a -> Resolver m b) WRAPPER (ResValue (StreamT m c)) where
  __encode resolver selection = ExceptT $ StreamT $ StreamState [] <$> runExceptT (__encode resolver selection)

instance (ArgumentsConstraint a, Monad m, Encoder b (KIND b) (ResValue m)) =>
         Encoder (a -> SubResolver m e c b) WRAPPER (SubResolveT m e c Value) where
  __encode (WithGQLKind resolver) selection = decodeArgs selection >>= handleResolver . resolver
    where
      handleResolver Event {channels, content} =
        ExceptT $ StreamT $ pure $ StreamState [channels] (Right $ encodeResolver selection . content)

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

encodeResolver :: (Monad m, Encoder a (KIND a) (ResValue m)) => (Text, Selection) -> Resolver m a -> ResValue m
encodeResolver selection@(fieldName, Selection {selectionPosition}) =
  resolverToResolveT selectionPosition fieldName >=> (`encode` selection)

decodeArgs :: (Monad m, ArgumentsConstraint a) => (Text, Selection) -> ResolveT m a
decodeArgs (_, Selection {selectionArguments}) = ExceptT $ pure $ decodeArguments selectionArguments

operatorToResolveT :: Monad m => ValidOperator' -> Resolver m a -> ResolveT m a
operatorToResolveT Operator' {operatorPosition, operatorName} = resolverToResolveT operatorPosition operatorName

encodeOperator :: (Monad m, EncodeCon m a v) => ResolveSel (ResolveT m v) -> EncodeOperator m a v
encodeOperator resSel rootResolver operator@Operator' {operatorSelection} =
  runExceptT (operatorToResolveT operator rootResolver >>= resSel operatorSelection . resolversBy)

resolveBySelection :: Monad m => ResolveSel (ResolveT m Value)
resolveBySelection selection resolvers = Object <$> mapM (selectResolver Null resolvers) selection

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
resolversBy = objectFieldResolvers . from
--------------------------------------------
