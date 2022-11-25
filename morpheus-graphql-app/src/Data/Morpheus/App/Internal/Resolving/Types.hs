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
    ResolverEntry,
    mkEnum,
    mkBoolean,
    mkFloat,
    mkInt,
    mkList,
    mkNull,
    mkString,
    mkObject,
    mkObjectMaybe,
    mkUnion,
    NamedResolverFun,
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.Resolver (MonadResolver)
import Data.Morpheus.Internal.Ext (Merge (..))
import Data.Morpheus.Internal.Utils (KeyOf (keyOf))
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    ScalarValue (..),
    TypeName,
    ValidValue,
    internal,
  )
import GHC.Show (Show (show))
import Relude hiding (show)

type ResolverMap (m :: Type -> Type) = HashMap TypeName (NamedResolver m)

type NamedResolverArg = [ValidValue]

type NamedResolverFun m = NamedResolverArg -> m [NamedResolverResult m]

data NamedResolver (m :: Type -> Type) = NamedResolver
  { resolverName :: TypeName,
    resolverFun :: NamedResolverFun m
  }

instance Show (NamedResolver m) where
  show NamedResolver {..} =
    "NamedResolver { name = " <> show resolverName <> " }"

newtype ObjectTypeResolver m = ObjectTypeResolver
  { objectFields :: HashMap FieldName (m (ResolverValue m))
  }

instance Show (ObjectTypeResolver m) where
  show _ = "ObjectTypeResolver {}"

data NamedResolverRef = NamedResolverRef
  { resolverTypeName :: TypeName,
    resolverArgument :: NamedResolverArg
  }
  deriving (Show)

data NamedResolverResult (m :: Type -> Type)
  = NamedObjectResolver (ObjectTypeResolver m)
  | NamedUnionResolver NamedResolverRef
  | NamedEnumResolver TypeName
  | NamedScalarResolver ScalarValue
  | NamedNullResolver

instance KeyOf TypeName (NamedResolver m) where
  keyOf = resolverName

instance Show (NamedResolverResult m) where
  show NamedObjectResolver {} = "NamedObjectResolver"
  show NamedUnionResolver {} = "NamedUnionResolver"
  show NamedEnumResolver {} = "NamedEnumResolver"
  show NamedNullResolver {} = "NamedNullResolver"
  show NamedScalarResolver {} = "NamedScalarResolver"

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
  merge (ObjectTypeResolver x) (ObjectTypeResolver y) =
    pure $ ObjectTypeResolver (HM.unionWith mergeFields x y)
    where
      mergeFields a b = (,) <$> a <*> b >>= uncurry merge

instance Show (ResolverValue m) where
  show ResNull = "ResNull"
  show (ResScalar x) = "ResScalar:" <> show x
  show (ResList xs) = "ResList:" <> show xs
  show (ResEnum name) = "ResEnum:" <> show name
  show (ResObject name _) = "ResObject:" <> show name
  show ResRef {} = "ResRef {}"
  show ResLazy {} = "ResLazy {}"

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

type ResolverEntry m = (FieldName, m (ResolverValue m))

--
mkString :: Text -> ResolverValue m
mkString = ResScalar . String

mkFloat :: Double -> ResolverValue m
mkFloat = ResScalar . Float

mkInt :: Int -> ResolverValue m
mkInt = ResScalar . Int

mkBoolean :: Bool -> ResolverValue m
mkBoolean = ResScalar . Boolean

mkList :: [ResolverValue m] -> ResolverValue m
mkList = ResList

mkNull :: ResolverValue m
mkNull = ResNull

mkEnum :: TypeName -> ResolverValue m
mkEnum = ResEnum

mkObject ::
  TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkObject name = mkObjectMaybe (Just name)

mkObjectMaybe ::
  Maybe TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkObjectMaybe name = ResObject name . ObjectTypeResolver . HM.fromList

mkUnion ::
  (Monad m) =>
  TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkUnion name fields =
  ResObject
    (Just name)
    ObjectTypeResolver {objectFields = HM.fromList fields}
