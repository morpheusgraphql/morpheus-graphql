{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Internal.Resolve.Explore
  ( useExploreResolvers,
    useObjectResolvers,
    EXPLORE,
  )
where

import Control.Monad.Except (MonadError)
import Data.Morpheus.App.Internal.Resolving
  ( ObjectTypeResolver (..),
    ResolverState,
    ResolverValue (..),
    mkEnum,
    mkObject,
    mkUnion,
    requireObject,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Directive
  ( toFieldRes,
    visitEnumName,
  )
import Data.Morpheus.Server.Deriving.Utils.DeriveGType
  ( DeriveWith,
    DerivingOptions (..),
    deriveValue,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (inputType)
import Data.Morpheus.Server.Deriving.Utils.Types
  ( ConsRep (..),
    DataType (..),
    FieldRep (..),
    isUnionRef,
  )
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
    UseResolver (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
    TypeRef (..),
  )
import GHC.Generics (Generic (Rep))
import Relude

convertNode ::
  gql a =>
  (MonadError GQLError m) =>
  UseDeriving gql val ->
  f a ->
  DataType (m (ResolverValue m)) ->
  ResolverValue m
convertNode
  drv
  proxy
  DataType
    { dataTypeName,
      tyIsUnion,
      tyCons = cons@ConsRep {consFields, consName}
    } = encodeTypeFields consFields
    where
      -- ENUM
      encodeTypeFields :: (MonadError GQLError m) => [FieldRep (m (ResolverValue m))] -> ResolverValue m
      encodeTypeFields [] = mkEnum (visitEnumName drv proxy consName)
      encodeTypeFields fields
        | not tyIsUnion = mkObject dataTypeName (toFieldRes drv proxy <$> fields)
      -- Type References --------------------------------------------------------------
      encodeTypeFields [FieldRep {fieldTypeRef, fieldValue}]
        | isUnionRef dataTypeName cons = ResLazy (ResObject (Just (typeConName fieldTypeRef)) <$> (fieldValue >>= requireObject))
      -- Inline Union Types ----------------------------------------------------------------------------
      encodeTypeFields fields = mkUnion consName (toFieldRes drv proxy <$> fields)

toOptions :: UseResolver res gql val -> DerivingOptions gql (res m) Identity (m (ResolverValue m))
toOptions UseResolver {..} =
  DerivingOptions
    { optApply = useEncodeResolver . runIdentity,
      optTypeData = useTypeData (dirGQL resDrv) . inputType
    }

useExploreResolvers ::
  (MonadError GQLError m, EXPLORE gql res m a) =>
  UseResolver res gql val ->
  a ->
  ResolverValue m
useExploreResolvers res v = convertNode (resDrv res) proxy (deriveValue (toOptions res) v)
  where
    proxy = Identity v

useObjectResolvers ::
  (MonadError GQLError m, EXPLORE gql res m a) =>
  UseResolver res gql val ->
  a ->
  ResolverState (ObjectTypeResolver m)
useObjectResolvers ctx value = requireObject (useExploreResolvers ctx value)

type EXPLORE gql res (m :: Type -> Type) a =
  ( Generic a,
    DeriveWith gql (res m) (m (ResolverValue m)) (Rep a),
    gql a
  )
