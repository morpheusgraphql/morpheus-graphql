{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Types
  ( ResolverMap,
    NamedResolver (..),
    NamedResolverResult (..),
    NamedResolverRef (..),
    ResolverValue (..),
    ObjectTypeResolver (..),
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.Internal.Ext (Merge (..))
import Data.Morpheus.Internal.Utils (KeyOf (keyOf))
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    ScalarValue,
    TypeName,
    ValidValue,
    internal,
  )
import GHC.Show (Show (show))
import Relude hiding (show)

type ResolverMap (m :: * -> *) = HashMap TypeName (NamedResolver m)

data NamedResolver (m :: * -> *) = NamedResolver
  { resolverName :: TypeName,
    resolver :: ValidValue -> m (NamedResolverResult m)
  }

instance Show (NamedResolver m) where
  show NamedResolver {..} =
    "NamedResolver { name = " <> show resolverName <> " }"

data ObjectTypeResolver m = ObjectTypeResolver
  { __typename :: TypeName,
    objectFields :: HashMap FieldName (m (ResolverValue m))
  }

instance Show (ObjectTypeResolver m) where
  show = undefined

data NamedResolverRef = NamedResolverRef
  { resolverTypeName :: TypeName,
    resolverArgument :: ValidValue
  }
  deriving (Show)

data NamedResolverResult (m :: * -> *)
  = NamedObjectResolver (ObjectTypeResolver m)
  | NamedUnionResolver NamedResolverRef
  | NamedEnumResolver TypeName

instance KeyOf TypeName (NamedResolver m) where
  keyOf = resolverName

data ResolverValue (m :: Type -> Type)
  = ResNull
  | ResScalar ScalarValue
  | ResList [ResolverValue m]
  | ResEnum TypeName
  | ResObject (Maybe TypeName) (ObjectTypeResolver m)
  | ResRef (m NamedResolverRef)
  | ResLazy (m (ResolverValue m))

instance
  ( Monad m,
    Applicative f,
    MonadError GQLError m
  ) =>
  Merge f (ObjectTypeResolver m)
  where
  merge (ObjectTypeResolver typeName x) (ObjectTypeResolver _ y) =
    pure $ ObjectTypeResolver typeName (HM.unionWith mergeFields x y)
    where
      mergeFields a b = (,) <$> a <*> b >>= uncurry merge

instance Show (ResolverValue m) where
  show = undefined

instance IsString (ResolverValue m) where
  fromString = ResScalar . fromString

instance
  ( Monad f,
    MonadError GQLError f,
    Merge f (ObjectTypeResolver m)
  ) =>
  Merge f (ResolverValue m)
  where
  merge ResNull ResNull = pure ResNull
  merge ResScalar {} x@ResScalar {} = pure x
  merge ResEnum {} x@ResEnum {} = pure x
  merge (ResObject n x) (ResObject _ y) = ResObject n <$> merge x y
  merge _ _ = throwError (internal "can't merge: incompatible resolvers")
