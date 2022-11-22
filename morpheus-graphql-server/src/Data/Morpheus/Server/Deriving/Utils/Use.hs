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
  )
where

import Data.Morpheus.App.Internal.Resolving (NamedResolver (..), ResolverState, ResolverValue)
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Server.Deriving.Utils.GTraversable (GmapProxy)
import Data.Morpheus.Server.Deriving.Utils.Kinded (CatType)
import Data.Morpheus.Server.Types.Directives
  ( GDirectiveUsages (..),
  )
import Data.Morpheus.Server.Types.Internal
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
  )
import Data.Morpheus.Server.Types.TypeName
  ( TypeFingerprint,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    TypeDefinition (..),
    TypeName,
    ValidValue,
    Value,
  )

data UseGQLType gql = UseGQLType
  { useFingerprint :: forall c a. gql a => CatType c a -> TypeFingerprint,
    useTypename :: forall c a. gql a => CatType c a -> TypeName,
    useTypeData :: forall c a. gql a => CatType c a -> TypeData,
    useDeriveType :: forall c a. gql a => CatType c a -> SchemaT c (TypeDefinition c CONST),
    useDeriveFieldArguments :: forall c a. gql a => CatType c a -> SchemaT c (Maybe (ArgumentsDefinition CONST))
  }

data UseValue val = UseValue
  { useEncodeValue :: forall a. val a => a -> GQLResult (Value CONST),
    useDecodeValue :: forall a. val a => ValidValue -> ResolverState a
  }

data UseResolver res gql val = UseResolver
  { useEncodeResolver :: forall a m. res m a => a -> m (ResolverValue m),
    resDrv :: UseDeriving gql val
  }

data UseDeriving gql val = UseDeriving
  { __directives :: forall f a. gql a => f a -> GDirectiveUsages gql val,
    dirArgs :: UseValue val,
    dirGQL :: UseGQLType gql
  }

data UseNamedResolver namedRes resFun gql val = UseNamedResolver
  { useNamedFieldResolver :: forall a m. resFun m a => a -> m (ResolverValue m),
    useDeriveNamedResolvers :: forall f a m. namedRes m a => f a -> ([NamedResolver m], [GmapProxy (namedRes m)]),
    namedDrv :: UseDeriving gql val
  }
