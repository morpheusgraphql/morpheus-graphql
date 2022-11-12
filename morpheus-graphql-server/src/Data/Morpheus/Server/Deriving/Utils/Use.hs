{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDirective (..),
    UseDeriveType (..),
    UseArguments (..),
    UseGQLType (..),
    useTypename,
  )
where

import Data.Morpheus.Server.Deriving.Schema.Internal
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
  ( Arguments,
    ArgumentsDefinition,
    CONST,
    OUT,
    TypeCategory (..),
    TypeName,
  )

useTypename :: (gqlType a) => UseGQLType gqlType -> CatType c a -> TypeName
useTypename gql proxy@InputType = __useTypename gql IN proxy
useTypename gql proxy@OutputType = __useTypename gql OUT proxy

data UseGQLType gql = UseGQLType
  { __useFingerprint :: forall f a. gql a => TypeCategory -> f a -> TypeFingerprint,
    __useTypename :: forall f a. gql a => TypeCategory -> f a -> TypeName,
    __useTypeData :: forall f a. gql a => f a -> TypeCategory -> TypeData
  }

data UseArguments args = UseArguments
  { useDeriveArguments :: forall f a. args a => f a -> SchemaT OUT (ArgumentsDefinition CONST),
    useEncodeArguments :: forall k a. args a => a -> SchemaT k (Arguments CONST)
  }

data UseDirective gql args = UseDirective
  { __directives :: forall f a. gql a => f a -> GDirectiveUsages gql args,
    dirArgs :: UseArguments args,
    dirGQL :: UseGQLType gql
  }

data UseDeriveType derive = UseDeriveType
  { useDeriveType :: forall c a. derive a => CatType c a -> SchemaT c (),
    useDeriveContent :: forall c a. derive a => CatType c a -> TyContentM c
  }
