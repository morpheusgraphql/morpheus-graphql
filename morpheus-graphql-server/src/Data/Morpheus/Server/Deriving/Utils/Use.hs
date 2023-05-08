{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    GQLTypeCTX (..),
    UseGQLValue (..),
    UseResolver (..),
    UseNamedResolver (..),
    UseRef (..),
    UseGQLType (..),
    GQLValueCTX (..),
    useDecodeArguments,
  )
where

import Data.Morpheus.App.Internal.Resolving (NamedResolver (..), ResolverState, ResolverValue)
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Server.Deriving.Utils.GScan (FreeCatType, ScanRef)
import Data.Morpheus.Server.Deriving.Utils.Types
import Data.Morpheus.Server.Types.Directives
  ( GDirectiveUsages (..),
  )
import Data.Morpheus.Server.Types.Internal
import Data.Morpheus.Server.Types.TypeName
  ( TypeFingerprint,
  )
import Data.Morpheus.Types.Internal.AST
  ( Arguments,
    ArgumentsDefinition,
    CONST,
    TypeName,
    TypeWrapper,
    VALID,
    ValidValue,
    Value,
  )
import Relude

data UseRef (c :: Type -> Constraint) where
  UseRef :: (c a) => CatType t a -> UseRef c

class UseGQLType ctx con | ctx -> con where
  useFingerprint :: (con a) => ctx -> CatType c a -> TypeFingerprint
  useTypename :: (con a) => ctx -> CatType c a -> TypeName
  useWrappers :: (con a) => ctx -> CatType c a -> TypeWrapper
  useDeriveNode :: (con a) => ctx -> CatType c a -> GQLResult (GQLTypeNode c)
  useDeriveFieldArgs :: (con a) => ctx -> CatType c a -> GQLResult (ArgumentsDefinition CONST)
  useExploreRef :: (con a) => ctx -> CatType c a -> [ScanRef FreeCatType con]

data GQLTypeCTX gql = GQLTypeCTX
  { __useFingerprint :: forall c a. (gql a) => CatType c a -> TypeFingerprint,
    __useTypename :: forall c a. (gql a) => CatType c a -> TypeName,
    __useTypeData :: forall c a. (gql a) => CatType c a -> TypeData,
    __useDeriveNode :: forall c a. (gql a) => CatType c a -> GQLResult (GQLTypeNode c),
    __useDeriveFieldArgs :: forall c a. (gql a) => CatType c a -> GQLResult (ArgumentsDefinition CONST),
    __useExploreRef :: forall c a. (gql a) => CatType c a -> [ScanRef FreeCatType gql]
  }

instance UseGQLType (GQLTypeCTX gql) gql where
  useFingerprint GQLTypeCTX {__useFingerprint} = __useFingerprint
  useTypename GQLTypeCTX {__useTypename} = __useTypename
  useWrappers GQLTypeCTX {__useTypeData} = gqlWrappers . __useTypeData
  useDeriveNode GQLTypeCTX {__useDeriveNode} = __useDeriveNode
  useDeriveFieldArgs GQLTypeCTX {__useDeriveFieldArgs} = __useDeriveFieldArgs
  useExploreRef GQLTypeCTX {__useExploreRef} = __useExploreRef

data GQLValueCTX val = GQLValueCTX
  { __useEncodeValue :: forall a. (val a) => a -> GQLResult (Value CONST),
    __useDecodeValue :: forall a. (val a) => ValidValue -> ResolverState a
  }

class UseGQLValue ctx con | ctx -> con where
  useEncodeValue :: (con a) => ctx -> a -> GQLResult (Value CONST)
  useDecodeValue :: (con a) => ctx -> ValidValue -> ResolverState a

instance UseGQLValue (GQLValueCTX val) val where
  useEncodeValue GQLValueCTX {__useEncodeValue} = __useEncodeValue
  useDecodeValue GQLValueCTX {__useDecodeValue} = __useDecodeValue

data UseDeriving gql val = UseDeriving
  { useDirectives :: forall f a. (gql a) => f a -> GDirectiveUsages gql val,
    __useValue :: GQLValueCTX val,
    __useGQL :: GQLTypeCTX gql
  }

instance UseGQLType (UseDeriving gql val) gql where
  useFingerprint = useFingerprint . __useGQL
  useTypename = useTypename . __useGQL
  useWrappers = useWrappers . __useGQL
  useDeriveNode = useDeriveNode . __useGQL
  useDeriveFieldArgs = useDeriveFieldArgs . __useGQL
  useExploreRef = useExploreRef . __useGQL

instance UseGQLValue (UseDeriving gql val) val where
  useEncodeValue = useEncodeValue . __useValue
  useDecodeValue = useDecodeValue . __useValue

data UseResolver res gql val = UseResolver
  { useEncodeResolver :: forall a m. (res m a) => a -> m (ResolverValue m),
    resDrv :: UseDeriving gql val
  }

instance UseGQLType (UseResolver res gql val) gql where
  useFingerprint = useFingerprint . resDrv
  useTypename = useTypename . resDrv
  useWrappers = useWrappers . resDrv
  useDeriveNode = useDeriveNode . resDrv
  useDeriveFieldArgs = useDeriveFieldArgs . resDrv
  useExploreRef = useExploreRef . resDrv

instance UseGQLValue (UseResolver res gql val) val where
  useEncodeValue = useEncodeValue . resDrv
  useDecodeValue = useDecodeValue . resDrv

data UseNamedResolver named fun gql val = UseNamedResolver
  { useNamedFieldResolver :: forall a m. (fun m a) => a -> m (ResolverValue m),
    useDeriveNamedResolvers :: forall f a m. (named m a) => f a -> [NamedResolver m],
    useDeriveNamedRefs :: forall f a m. (named m a) => f a -> [ScanRef FreeCatType (named m)],
    namedDrv :: UseDeriving gql val
  }

instance UseGQLType (UseNamedResolver named res gql val) gql where
  useFingerprint = useFingerprint . namedDrv
  useTypename = useTypename . namedDrv
  useWrappers = useWrappers . namedDrv
  useDeriveNode = useDeriveNode . namedDrv
  useDeriveFieldArgs = useDeriveFieldArgs . namedDrv
  useExploreRef = useExploreRef . namedDrv

useDecodeArguments :: (val a) => UseDeriving gql val -> Arguments VALID -> ResolverState a
useDecodeArguments ctx = useDecodeValue ctx . argumentsToObject
