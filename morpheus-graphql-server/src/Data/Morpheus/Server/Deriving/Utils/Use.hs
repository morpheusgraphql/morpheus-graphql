{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
    UseValue (..),
    UseResolver (..),
    UseNamedResolver (..),
    UseRef (..),
  )
where

import Data.Morpheus.App.Internal.Resolving (NamedResolver (..), ResolverState, ResolverValue)
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Server.Deriving.Utils.GScan (ScanRef)
import Data.Morpheus.Server.Deriving.Utils.Types
import Data.Morpheus.Server.Types.Directives
  ( GDirectiveUsages (..),
  )
import Data.Morpheus.Server.Types.Internal
import Data.Morpheus.Server.Types.TypeName
  ( TypeFingerprint,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    TypeName,
    ValidValue,
    Value,
  )
import Relude

data UseRef (c :: Type -> Constraint) where
  UseRef :: (c a) => CatType t a -> UseRef c

data UseGQLType gql = UseGQLType
  { useFingerprint :: forall c a. (gql a) => CatType c a -> TypeFingerprint,
    useTypename :: forall c a. (gql a) => CatType c a -> TypeName,
    useTypeData :: forall c a. (gql a) => CatType c a -> TypeData,
    useDeriveNode :: forall c a. (gql a) => CatType c a -> GQLResult (GQLTypeNode c),
    useDeriveFieldArguments :: forall c a. (gql a) => CatType c a -> GQLResult (ArgumentsDefinition CONST),
    useExploreRef :: forall c a. (gql a) => CatType c a -> [ScanRef gql]
  }

data UseValue val = UseValue
  { useEncodeValue :: forall a. (val a) => a -> GQLResult (Value CONST),
    useDecodeValue :: forall a. (val a) => ValidValue -> ResolverState a
  }

data UseDeriving gql val = UseDeriving
  { useDirectives :: forall f a. (gql a) => f a -> GDirectiveUsages gql val,
    useValue :: UseValue val,
    useGQL :: UseGQLType gql
  }

data UseResolver res gql val = UseResolver
  { useEncodeResolver :: forall a m. (res m a) => a -> m (ResolverValue m),
    resDrv :: UseDeriving gql val
  }

data UseNamedResolver named fun gql val = UseNamedResolver
  { useNamedFieldResolver :: forall a m. (fun m a) => a -> m (ResolverValue m),
    useDeriveNamedResolvers :: forall f a m. (named m a) => f a -> [NamedResolver m],
    useDeriveNamedRefs :: forall f a m. (named m a) => f a -> [ScanRef (named m)],
    namedDrv :: UseDeriving gql val
  }
