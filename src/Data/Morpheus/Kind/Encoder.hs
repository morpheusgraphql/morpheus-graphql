{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Kind.Encoder where

import           Control.Monad.Trans                        (lift)
import           Control.Monad.Trans.Except
import           Data.Morpheus.Error.Internal               (internalErrorIO)
import           Data.Morpheus.Error.Selection              (fieldNotResolved, subfieldsNotSelected)
import           Data.Morpheus.Generics.DeriveResolvers     (DeriveResolvers (..), resolveBySelection,
                                                             resolveBySelectionM, resolversBy)
import           Data.Morpheus.Generics.EnumRep             (EnumRep (..))
import           Data.Morpheus.Generics.UnionResolvers      (UnionResolvers (..), lookupSelectionByType)
import qualified Data.Morpheus.Kind.GQLArgs                 as Args (GQLArgs (..))
import           Data.Morpheus.Kind.GQLKinds                (EncodeObjectConstraint, EncodeUnionConstraint, Encode_,
                                                             EnumConstraint)
import qualified Data.Morpheus.Kind.GQLScalar               as S (GQLScalar (..))
import           Data.Morpheus.Kind.GQLType                 (GQLType (..))
import           Data.Morpheus.Kind.Internal                (ENUM, KIND, OBJECT, SCALAR, UNION, WRAPPER)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..))
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO, failResolveIO)
import           Data.Morpheus.Types.Internal.Value         (ScalarValue (..), Value (..))
import           Data.Morpheus.Types.Resolver               ((::->), (::->>), Resolver (..), Result (..))
import           Data.Proxy                                 (Proxy (..))
import           Data.Text                                  (Text, pack)
import           GHC.Generics

type MResult = Result Value

type QueryResult = Value

-- { ENCODE }
class Encoder a kind toValue where
  __encode :: Proxy kind -> Encode_ a toValue

_encode ::
     forall a v. Encoder a (KIND a) v
  => Encode_ a v
_encode = __encode (Proxy @(KIND a))

-- Encode Queries
instance (S.GQLScalar a, GQLType a) => Encoder a SCALAR QueryResult where
  __encode _ _ = pure . S.encode

instance EnumConstraint a => Encoder a ENUM QueryResult where
  __encode _ _ = pure . Scalar . String . encodeRep . from

instance EncodeObjectConstraint a QueryResult => Encoder a OBJECT QueryResult where
  __encode _ = encodeObject
    where
      encodeObject :: Encode_ a QueryResult
      encodeObject (_, Selection {selectionRec = SelectionSet selection'}) value =
        resolveBySelection selection' (__typename : resolversBy value)
        where
          __typename = ("__typename", const $ return $ Scalar $ String $typeID (Proxy @a))
      encodeObject (key, Selection {selectionPosition = position'}) _ =
        failResolveIO $ subfieldsNotSelected key "" position'

instance EncodeUnionConstraint a QueryResult => Encoder a UNION QueryResult where
  __encode _ = encodeUnion
    where
      encodeUnion :: Encode_ a QueryResult
      encodeUnion (key', sel@Selection {selectionRec = UnionSelection selections'}) value =
        resolver (key', sel {selectionRec = SelectionSet (lookupSelectionByType type' selections')})
        where
          (type', resolver) = currentResolver (from value)
      encodeUnion _ _ = internalErrorIO "union Resolver only should recieve UnionSelection"

instance Encoder a (KIND a) QueryResult => Encoder (Maybe a) WRAPPER QueryResult where
  __encode _ = encodeMaybe Null _encode

instance Encoder a (KIND a) QueryResult => Encoder [a] WRAPPER QueryResult where
  __encode _ = encodeList _encode
    where
      encodeList :: Encode_ a Value -> Encode_ [a] Value
      encodeList _ (_, Selection {selectionRec = SelectionField {}}) _ = pure $ List []
      encodeList f query list                                          = List <$> mapM (f query) list

instance (Encoder a (KIND a) QueryResult, Args.GQLArgs p) => Encoder (p ::-> a) WRAPPER QueryResult where
  __encode _ selection'@(key', Selection {selectionArguments = astArgs', selectionPosition = position'}) (Resolver resolver) = do
    args <- ExceptT $ pure $ Args.decode astArgs'
    liftResolver position' key' (resolver args) >>= _encode selection'

-- Encode Mutations and Subscriptions
instance (S.GQLScalar a, GQLType a) => Encoder a SCALAR MResult where
  __encode _ _ = pure . pure . S.encode

instance EnumConstraint a => Encoder a ENUM MResult where
  __encode _ _ = pure . pure . Scalar . String . encodeRep . from

instance EncodeObjectConstraint a MResult => Encoder a OBJECT MResult where
  __encode _ = encodeObject
    where
      encodeObject :: Encode_ a MResult
      encodeObject (_, Selection {selectionRec = SelectionSet selection'}) value =
        resolveBySelectionM selection' (__typename : resolversBy value)
        where
          __typename = ("__typename", const $ return $ return $ Scalar $ String $typeID (Proxy @a))
      encodeObject (key, Selection {selectionPosition = position'}) _ =
        failResolveIO $ subfieldsNotSelected key "" position'

instance EncodeUnionConstraint a MResult => Encoder a UNION MResult where
  __encode _ = encodeUnion
    where
      encodeUnion :: Encode_ a MResult
      encodeUnion (key', sel@Selection {selectionRec = UnionSelection selections'}) value =
        resolver (key', sel {selectionRec = SelectionSet (lookupSelectionByType type' selections')})
        where
          (type', resolver) = currentResolver (from value)
      encodeUnion _ _ = internalErrorIO "union Resolver only should recieve UnionSelection"

instance (Encoder a (KIND a) MResult, Args.GQLArgs p) => Encoder (p ::->> a) WRAPPER MResult where
  __encode _ selection'@(key', Selection {selectionArguments = astArgs', selectionPosition = position'}) (Resolver resolver) = do
    args <- ExceptT $ pure $ Args.decode astArgs'
    Result value1 effects1 <- liftResolver position' key' (resolver args)
    Result value2 effects2 <- _encode selection' value1
    return $ Result value2 (effects1 ++ effects2)

instance (Encoder a (KIND a) MResult, Args.GQLArgs p) => Encoder (p ::-> a) WRAPPER MResult where
  __encode _ selection'@(key', Selection {selectionArguments = astArgs', selectionPosition = position'}) (Resolver resolver) = do
    args <- ExceptT $ pure $ Args.decode astArgs'
    liftResolver position' key' (resolver args) >>= _encode selection'

instance Encoder a (KIND a) MResult => Encoder (Maybe a) WRAPPER MResult where
  __encode _ = encodeMaybe (return Null) _encode

instance Encoder a (KIND a) MResult => Encoder [a] WRAPPER MResult where
  __encode _ = encodeListM _encode
    where
      encodeListM :: Encode_ a (Result Value) -> Encode_ [a] (Result Value)
      encodeListM _ (_, Selection {selectionRec = SelectionField {}}) _ = pure $ pure (List [])
      encodeListM f query list = do
        value' <- mapM (f query) list
        return $ Result (List (map resultValue value')) (concatMap resultEffects value')

-- GENERIC Instances
instance Encoder a (KIND a) res => DeriveResolvers (K1 s a) res where
  deriveResolvers key' (K1 src) = [(key', (`_encode` src))]

instance (GQLType a, Encoder a (KIND a) res) => UnionResolvers (K1 s a) res where
  currentResolver (K1 src) = (typeID (Proxy @a), (`_encode` src))

liftResolver :: Int -> Text -> IO (Either String a) -> ResolveIO a
liftResolver position' typeName' x = do
  result <- lift x
  case result of
    Left message' -> failResolveIO $ fieldNotResolved position' typeName' (pack message')
    Right value   -> pure value

encodeMaybe :: res -> Encode_ a res -> Encode_ (Maybe a) res
encodeMaybe defaultValue _ _ Nothing = pure defaultValue
encodeMaybe _ f query (Just value)   = f query value
