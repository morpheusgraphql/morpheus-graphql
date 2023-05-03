{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Internal.Resolver
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
import Data.Morpheus.Generic
  ( GRep,
    GRepContext (..),
    GRepValue (..),
    deriveValue,
  )
import Data.Morpheus.Server.Deriving.Internal.Schema.Directive
  ( toFieldRes,
    visitEnumName,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (inputType)
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseDeriving (..),
    UseGQLType (..),
    UseResolver (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError,
  )
import GHC.Generics (Generic (Rep))
import Relude

convertNode ::
  (MonadError GQLError m, gql a) =>
  UseDeriving gql val ->
  f a ->
  GRepValue (m (ResolverValue m)) ->
  ResolverValue m
convertNode drv proxy GRepValueEnum {..} = mkEnum (visitEnumName drv proxy enumVariantName)
convertNode drv proxy GRepValueObject {..} = mkObject objectTypeName (toFieldRes drv proxy <$> objectFields)
convertNode drv proxy GRepValueUnion {..} = mkUnion unionVariantName (toFieldRes drv proxy <$> unionFields)
convertNode _ _ GRepValueUnionRef {..} = ResLazy (ResObject (Just unionRefTypeName) <$> (unionRefValue >>= requireObject))

toOptions :: UseResolver res gql val -> GRepContext gql (res m) Identity (m (ResolverValue m))
toOptions ctx =
  GRepContext
    { grepFun = useEncodeResolver ctx . runIdentity,
      grepTypename = useTypename ctx . inputType,
      grepWrappers = useWrappers ctx . inputType
    }

useExploreResolvers ::
  (MonadError GQLError m, EXPLORE gql res m a) =>
  UseResolver res gql val ->
  a ->
  ResolverValue m
useExploreResolvers res v = convertNode (resDrv res) (Identity v) (deriveValue (toOptions res) v)

useObjectResolvers ::
  (MonadError GQLError m, EXPLORE gql res m a) =>
  UseResolver res gql val ->
  a ->
  ResolverState (ObjectTypeResolver m)
useObjectResolvers ctx value = requireObject (useExploreResolvers ctx value)

type EXPLORE gql res (m :: Type -> Type) a =
  ( Generic a,
    GRep gql (res m) (m (ResolverValue m)) (Rep a),
    gql a
  )
