{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DefaultSignatures        #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module Data.Morpheus.Types.GQLOperator
  ( GQLQuery(..)
  , GQLMutation(..)
  , GQLSubscription(..)
  ) where

import           Data.Morpheus.Resolve.Encode               (ObjectFieldResolvers (..), resolveBySelection,
                                                             resolveBySelectionM, resolversBy)
import           Data.Morpheus.Resolve.Generics.TypeRep     (ObjectRep (..), TypeUpdater, resolveTypes)
import           Data.Morpheus.Resolve.Introspect           (introspectOutputType)
import           Data.Morpheus.Schema.Schema                (Schema, Type, findType, initSchema)
import           Data.Morpheus.Types.GQLArgs                (GQLArgs)
import           Data.Morpheus.Types.Internal.AST.Selection (SelectionSet)
import           Data.Morpheus.Types.Internal.Data          (DataArguments, DataType (..), DataTypeLib (..),
                                                             initTypeLib)
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO, SchemaValidation)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Morpheus.Types.Resolver               ((::->), Resolver (..), WithEffect (..))
import           Data.Proxy
import           Data.Text                                  (Text)
import           Data.Typeable                              (Typeable, typeRep, typeRepFingerprint)
import           GHC.Generics

type QResult = Value

type MResult = WithEffect Value

type Encode a r = a -> SelectionSet -> ResolveIO r

type EncodeCon a r = (Generic a, ObjectFieldResolvers (Rep a) r)

type IntroCon a = (ObjectRep (Rep a) DataArguments, Typeable a)

operatorType :: Typeable t => Proxy t -> Text -> a -> (Text, DataType a)
operatorType proxy name' fields' =
  ( name'
  , DataType
      {typeData = fields', typeName = name', typeFingerprint = typeRepFingerprint $ typeRep proxy, typeDescription = ""})

newtype TypeArgs = TypeArgs
  { name :: Text
  } deriving (Generic, GQLArgs)

data SystemQuery = SystemQuery
  { __type   :: TypeArgs ::-> Maybe Type
  , __schema :: Schema
  } deriving (Generic)

systemQuery :: DataTypeLib -> SystemQuery
systemQuery lib =
  SystemQuery {__type = Resolver $ \TypeArgs {name} -> return $ Right $ findType name lib, __schema = initSchema lib}

-- | derives GQL Query Operator
class GQLQuery a where
  encodeQuery :: DataTypeLib -> Encode a QResult
  default encodeQuery :: EncodeCon a QResult =>
    DataTypeLib -> Encode a QResult
  encodeQuery types rootResolver sel =
    resolveBySelection sel (resolversBy (systemQuery types) ++ resolversBy rootResolver)
  querySchema :: a -> SchemaValidation DataTypeLib
  default querySchema :: IntroCon a =>
    a -> SchemaValidation DataTypeLib
  querySchema _ = resolveTypes queryType (introspectOutputType (Proxy @Schema) : stack')
    where
      queryType = initTypeLib (operatorType (Proxy @a) "Query" (__fields ++ fields'))
      __fields = map fst $ objectFieldTypes $ Proxy @(Rep SystemQuery)
      (fields', stack') = unzip $ objectFieldTypes (Proxy @(Rep a))

-- | derives GQL Subscription Mutation
class GQLMutation a where
  encodeMutation :: Encode a MResult
  default encodeMutation :: EncodeCon a MResult =>
    Encode a MResult
  encodeMutation rootResolver sel = resolveBySelectionM sel $ resolversBy rootResolver
  mutationSchema :: a -> TypeUpdater
  default mutationSchema :: IntroCon a =>
    a -> TypeUpdater
  mutationSchema _ initialType = resolveTypes mutationType types'
    where
      mutationType = initialType {mutation = Just $ operatorType (Proxy @a) "Mutation" fields'}
      (fields', types') = unzip $ objectFieldTypes (Proxy :: Proxy (Rep a))

-- | derives GQL Subscription Operator
class GQLSubscription a where
  encodeSubscription :: Encode a MResult
  default encodeSubscription :: EncodeCon a MResult =>
    Encode a MResult
  encodeSubscription rootResolver sel = resolveBySelectionM sel $ resolversBy rootResolver
  subscriptionSchema :: a -> TypeUpdater
  default subscriptionSchema :: IntroCon a =>
    a -> TypeUpdater
  subscriptionSchema _ initialType = resolveTypes subscriptionType types'
    where
      subscriptionType = initialType {subscription = Just $ operatorType (Proxy @a) "Subscription" fields'}
      (fields', types') = unzip $ objectFieldTypes (Proxy :: Proxy (Rep a))

instance GQLMutation () where
  encodeMutation _ _ = pure $ pure Null
  mutationSchema _ = return

instance GQLSubscription () where
  encodeSubscription _ _ = pure $ pure Null
  subscriptionSchema _ = return
