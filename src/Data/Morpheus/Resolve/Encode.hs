{-# LANGUAGE ConstraintKinds       #-}
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

module Data.Morpheus.Resolve.Encode
  ( ObjectFieldResolvers(..)
  , resolveBySelection
  , resolversBy
  , QueryResult
  ) where

import           Control.Monad.Trans                        (lift)
import           Control.Monad.Trans.Except
import           Data.Maybe                                 (fromMaybe)
import           Data.Proxy                                 (Proxy (..))
import           Data.Text                                  (Text, pack)
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Internal               (internalErrorT)
import           Data.Morpheus.Error.Selection              (fieldNotResolved, subfieldsNotSelected)
import           Data.Morpheus.Kind                         (ENUM, KIND, OBJECT, SCALAR, UNION, WRAPPER)
import           Data.Morpheus.Resolve.Decode               (ArgumentsConstraint, decodeArguments)
import           Data.Morpheus.Resolve.Generics.EnumRep     (EnumRep (..))
import           Data.Morpheus.Types.GQLScalar              (GQLScalar (..))
import           Data.Morpheus.Types.GQLType                (GQLType (__typeName))
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..), SelectionSet)
import           Data.Morpheus.Types.Internal.Validation    (ResolveT, failResolveT)
import           Data.Morpheus.Types.Internal.Value         (ScalarValue (..), Value (..))
import           Data.Morpheus.Types.Resolver               (Effect (..), EffectT (..), Resolver (..))

type SelectRes m a = [(Text, (Text, Selection) -> ResolveT m a)] -> (Text, Selection) -> ResolveT m (Text, a)

type ResolveSel m a = [(Text, Selection)] -> [(Text, (Text, Selection) -> ResolveT m a)] -> ResolveT m a

--
--  OBJECT
-- | Derives resolvers by object fields
class ObjectFieldResolvers f m where
  objectFieldResolvers :: Text -> f a -> [(Text, (Text, Selection) -> ResolveT m Value)]

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

unwrapMonadTuple :: Monad m => (Text, m a) -> m (Text, a)
unwrapMonadTuple (text, ioa) = ioa >>= \x -> pure (text, x)

selectResolver :: Monad m => a -> SelectRes m a
selectResolver defaultValue resolvers' (key', selection') =
  case selectionRec selection' of
    SelectionAlias name' aliasSelection' ->
      unwrapMonadTuple (key', lookupResolver name' (selection' {selectionRec = aliasSelection'}))
    _ -> unwrapMonadTuple (key', lookupResolver key' selection')
  where
    lookupResolver resolverKey' sel =
      (fromMaybe (const $ return $defaultValue) $ lookup resolverKey' resolvers') (key', sel)

--
-- UNION
--
class UnionResolvers f m where
  unionResolvers :: f a -> (Text, (Text, Selection) -> ResolveT m Value)

instance UnionResolvers f res => UnionResolvers (M1 S s f) res where
  unionResolvers (M1 x) = unionResolvers x

instance UnionResolvers f res => UnionResolvers (M1 D c f) res where
  unionResolvers (M1 x) = unionResolvers x

instance UnionResolvers f res => UnionResolvers (M1 C c f) res where
  unionResolvers (M1 x) = unionResolvers x

instance (UnionResolvers a res, UnionResolvers b res) => UnionResolvers (a :+: b) res where
  unionResolvers (L1 x) = unionResolvers x
  unionResolvers (R1 x) = unionResolvers x

type ObjectConstraint a m = (Monad m, Generic a, GQLType a, ObjectFieldResolvers (Rep a) m)

type UnionConstraint a m = (Monad m, Generic a, GQLType a, UnionResolvers (Rep a) m)

type EnumConstraint a = (Generic a, EnumRep (Rep a))

type QueryResult = Value

newtype WithGQLKind a b = WithGQLKind
  { resolverValue :: a
  }

type GQLKindOf a = WithGQLKind a (KIND a)

encode ::
     forall a m. Encoder a (KIND a) m
  => a
  -> (Text, Selection)
  -> ResolveT m Value
encode resolver = __encode (WithGQLKind resolver :: GQLKindOf a)

class Encoder a kind m where
  __encode :: WithGQLKind a kind -> (Text, Selection) -> ResolveT m Value

--
-- SCALAR
--
instance (GQLScalar a, Monad m) => Encoder a SCALAR m where
  __encode = pure . pure . Scalar . serialize . resolverValue

--
-- ENUM
--
instance (EnumConstraint a, Monad m) => Encoder a ENUM m where
  __encode = pure . pure . Scalar . String . encodeRep . from . resolverValue

--
--  OBJECTS
--
instance ObjectConstraint a m => Encoder a OBJECT m where
  __encode (WithGQLKind value) (_, Selection {selectionRec = SelectionSet selection'}) =
    resolveBySelection selection' (__typenameResolver : resolversBy value)
    where
      __typenameResolver = ("__typename", const $ return $ Scalar $ String $ __typeName (Proxy @a))
  __encode _ (key, Selection {selectionPosition}) = failResolveT $ subfieldsNotSelected key "" selectionPosition

resolveBySelection :: Monad m => ResolveSel m Value
resolveBySelection selection resolvers = Object <$> mapM (selectResolver Null resolvers) selection

resolversBy ::
     (Generic a, Monad m, ObjectFieldResolvers (Rep a) m) => a -> [(Text, (Text, Selection) -> ResolveT m Value)]
resolversBy = objectFieldResolvers "" . from

instance Encoder a (KIND a) res => ObjectFieldResolvers (K1 s a) res where
  objectFieldResolvers key' (K1 src) = [(key', encode src)]

-- | Resolves and encodes UNION,
-- Handles all operators: Query, Mutation and Subscription,
instance UnionConstraint a m => Encoder a UNION m where
  __encode (WithGQLKind value) (key', sel@Selection {selectionRec = UnionSelection selections'}) =
    resolver (key', sel {selectionRec = SelectionSet lookupSelection})
    where
      lookupSelection :: SelectionSet
      -- SPEC: if there is no any fragment that supports current object Type GQL returns {}
      lookupSelection = fromMaybe [] $ lookup typeName selections'
      (typeName, resolver) = unionResolvers (from value)
  __encode _ _ = internalErrorT "union Resolver only should recieve UnionSelection"

instance (GQLType a, Encoder a (KIND a) res) => UnionResolvers (K1 s a) res where
  unionResolvers (K1 src) = (__typeName (Proxy @a), encode src)

--
--  RESOLVER: ::-> and ::->>
--
-- | Handles all operators: Query, Mutation and Subscription,
-- if you use it with Mutation or Subscription all effects inside will be lost
instance (Monad m, Encoder a (KIND a) m, ArgumentsConstraint p) => Encoder (Resolver m p a) WRAPPER m where
  __encode (WithGQLKind (Resolver resolver)) selection'@(fieldName, Selection {selectionArguments, selectionPosition}) = do
    args <- ExceptT $ pure $ decodeArguments selectionArguments
    liftResolver args >>= (`encode` selection')
    where
      liftResolver args = do
        result <- lift (resolver args)
        case result of
          Left message' -> failResolveT $ fieldNotResolved selectionPosition fieldName (pack message')
          Right value   -> pure value

-- packs Monad in EffectMonad
instance (Monad m, Encoder a (KIND a) m, ArgumentsConstraint p) => Encoder (p -> a) WRAPPER m where
  __encode (WithGQLKind resolver) selection'@(_, Selection {selectionArguments}) =
    case decodeArguments selectionArguments of
      Left message -> failResolveT message
      Right value  -> encode (resolver value) selection'

-- packs Monad in EffectMonad
instance (Monad m, Encoder a (KIND a) m, ArgumentsConstraint p) => Encoder (Resolver m p a) WRAPPER (EffectT m c) where
  __encode resolver selection = ExceptT $ EffectT $ Effect [] <$> runExceptT (__encode resolver selection)

--
-- MAYBE
--
instance (Monad m, Encoder a (KIND a) m) => Encoder (Maybe a) WRAPPER m where
  __encode (WithGQLKind Nothing)      = const $ pure Null
  __encode (WithGQLKind (Just value)) = encode value

--
-- LIST
--
instance (Monad m, Encoder a (KIND a) m) => Encoder [a] WRAPPER m where
  __encode (WithGQLKind list) query = List <$> mapM (`__encode` query) (map WithGQLKind list :: [GQLKindOf a])
