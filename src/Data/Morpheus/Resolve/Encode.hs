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
  ( encode
  ) where

import           Control.Monad.Trans                            (lift)
import           Control.Monad.Trans.Except
import           Data.Morpheus.Error.Internal                   (internalErrorIO)
import           Data.Morpheus.Error.Selection                  (fieldNotResolved, subfieldsNotSelected)
import           Data.Morpheus.Kind                             (ENUM, KIND, OBJECT, SCALAR, UNION, WRAPPER)
import           Data.Morpheus.Resolve.Generics.DeriveResolvers (ObjectFieldResolvers (..), UnionResolvers (..),
                                                                 lookupSelectionByType, resolveBySelection,
                                                                 resolveBySelectionM, resolversBy)
import           Data.Morpheus.Resolve.Generics.EnumRep         (EnumRep (..))
import qualified Data.Morpheus.Types.GQLArgs                    as Args (GQLArgs (..))
import           Data.Morpheus.Types.GQLScalar                  (GQLScalar (..))
import           Data.Morpheus.Types.GQLType                    (GQLType (__typeName))
import           Data.Morpheus.Types.Internal.AST.Selection     (Selection (..), SelectionRec (..))
import           Data.Morpheus.Types.Internal.Base              (Position)
import           Data.Morpheus.Types.Internal.Validation        (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.Internal.Value             (ScalarValue (..), Value (..))
import           Data.Morpheus.Types.Resolver                   ((::->), (::->>), Resolver (..), WithEffect (..))
import           Data.Proxy                                     (Proxy (..))
import           Data.Text                                      (Text, pack)
import           GHC.Generics

type ObjectConstraint a b = (Generic a, GQLType a, ObjectFieldResolvers (Rep a) b)

type UnionConstraint a res = (Generic a, GQLType a, UnionResolvers (Rep a) res)

type EnumConstraint a = (Generic a, EnumRep (Rep a))

type MResult = WithEffect Value

type QueryResult = Value

newtype WithGQLKind a b = WithGQLKind
  { resolverValue :: a
  }

type GQLKindOf a = WithGQLKind a (KIND a)

encode ::
     forall a b. Encoder a (KIND a) b
  => a
  -> (Text, Selection)
  -> ResolveIO b
encode resolver = __encode (WithGQLKind resolver :: GQLKindOf a)

class Encoder a kind toValue where
  __encode :: WithGQLKind a kind -> (Text, Selection) -> ResolveIO toValue

--
-- SCALAR
--
instance GQLScalar a => Encoder a SCALAR QueryResult where
  __encode = pure . pure . Scalar . serialize . resolverValue

instance GQLScalar a => Encoder a SCALAR MResult where
  __encode value selection = pure <$> __encode value selection

--
-- ENUM
--
instance EnumConstraint a => Encoder a ENUM QueryResult where
  __encode = pure . pure . Scalar . String . encodeRep . from . resolverValue

instance EnumConstraint a => Encoder a ENUM MResult where
  __encode value selection = pure <$> __encode value selection

--
--  OBJECTS
--
instance ObjectConstraint a QueryResult => Encoder a OBJECT QueryResult where
  __encode (WithGQLKind value) (_, Selection {selectionRec = SelectionSet selection'}) =
    resolveBySelection selection' (__typenameResolver : resolversBy value)
    where
      __typenameResolver = ("__typename", const $ return $ Scalar $ String $ __typeName (Proxy @a))
  __encode _ (key, Selection {selectionPosition}) = failResolveIO $ subfieldsNotSelected key "" selectionPosition

instance ObjectConstraint a MResult => Encoder a OBJECT MResult where
  __encode (WithGQLKind value) (_, Selection {selectionRec = SelectionSet selection'}) =
    resolveBySelectionM selection' (__typenameResolver : resolversBy value)
    where
      __typenameResolver = ("__typename", const $ return $ return $ Scalar $ String $ __typeName (Proxy @a))
  __encode _ (key, Selection {selectionPosition}) = failResolveIO $ subfieldsNotSelected key "" selectionPosition

instance Encoder a (KIND a) res => ObjectFieldResolvers (K1 s a) res where
  objectFieldResolvers key' (K1 src) = [(key', encode src)]

-- | Resolves and encodes UNION,
-- Handles all operators: Query, Mutation and Subscription,
instance UnionConstraint a res => Encoder a UNION res where
  __encode (WithGQLKind value) (key', sel@Selection {selectionRec = UnionSelection selections'}) =
    resolver (key', sel {selectionRec = SelectionSet (lookupSelectionByType type' selections')})
    where
      (type', resolver) = unionResolvers (from value)
  __encode _ _ = internalErrorIO "union Resolver only should recieve UnionSelection"

instance (GQLType a, Encoder a (KIND a) res) => UnionResolvers (K1 s a) res where
  unionResolvers (K1 src) = (__typeName (Proxy @a), encode src)

--
--  RESOLVER: ::-> and ::->>
--
-- | Handles all operators: Query, Mutation and Subscription,
-- if you use it with Mutation or Subscription all effects inside will be lost
instance (Encoder a (KIND a) res, Args.GQLArgs p) => Encoder (p ::-> a) WRAPPER res where
  __encode (WithGQLKind (Resolver resolver)) selection'@(key', Selection {selectionArguments, selectionPosition}) = do
    args <- ExceptT $ pure $ Args.decode selectionArguments
    liftResolver selectionPosition key' (resolver args) >>= (`encode` selection')

-- | resolver with effect, concatenates sideEffects of child resolvers
instance (Encoder a (KIND a) MResult, Args.GQLArgs p) => Encoder (p ::->> a) WRAPPER MResult where
  __encode (WithGQLKind (Resolver resolver)) selection'@(key', Selection {selectionArguments, selectionPosition}) = do
    args <- ExceptT $ pure $ Args.decode selectionArguments
    WithEffect effects1 value1 <- liftResolver selectionPosition key' (resolver args)
    WithEffect effects2 value2 <- __encode (WithGQLKind value1 :: GQLKindOf a) selection'
    return $ WithEffect (effects1 ++ effects2) value2

liftResolver :: Position -> Text -> IO (Either String a) -> ResolveIO a
liftResolver position' typeName' x = do
  result <- lift x
  case result of
    Left message' -> failResolveIO $ fieldNotResolved position' typeName' (pack message')
    Right value   -> pure value

--
-- MAYBE
--
instance Encoder a (KIND a) QueryResult => Encoder (Maybe a) WRAPPER QueryResult where
  __encode (WithGQLKind Nothing)      = const $ pure Null
  __encode (WithGQLKind (Just value)) = encode value

instance Encoder a (KIND a) MResult => Encoder (Maybe a) WRAPPER MResult where
  __encode (WithGQLKind Nothing)      = const $ pure $ pure Null
  __encode (WithGQLKind (Just value)) = encode value

--
-- LIST
--
instance Encoder a (KIND a) QueryResult => Encoder [a] WRAPPER QueryResult where
  __encode list query = List <$> mapGQLList list query

instance Encoder a (KIND a) MResult => Encoder [a] WRAPPER MResult where
  __encode list query = do
    value' <- mapGQLList list query
    return $ WithEffect (concatMap resultEffects value') (List (map resultValue value'))

mapGQLList ::
     forall a b. Encoder a (KIND a) b
  => GQLKindOf [a]
  -> (Text, Selection)
  -> ResolveIO [b]
mapGQLList (WithGQLKind list) query = mapM (`__encode` query) (map WithGQLKind list :: [GQLKindOf a])
