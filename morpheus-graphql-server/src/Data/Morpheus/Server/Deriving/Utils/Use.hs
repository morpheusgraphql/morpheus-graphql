{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDirective (..),
    UseArguments (..),
    UseGQLType (..),
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
    TypeDefinition (..),
    TypeName,
  )

data UseGQLType gql = UseGQLType
  { useFingerprint :: forall c a. gql a => CatType c a -> TypeFingerprint,
    useTypename :: forall c a. gql a => CatType c a -> TypeName,
    useTypeData :: forall c a. gql a => CatType c a -> TypeData,
    useDeriveType :: forall c a. gql a => CatType c a -> SchemaT c (TypeDefinition c CONST),
    useDeriveContent :: forall c a. gql a => CatType c a -> TyContentM c
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
