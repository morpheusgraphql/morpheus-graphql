{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.Morpheus.App.NamedResolvers
  ( ref,
    object,
    variant,
    list,
    refs,
    enum,
    queryResolvers,
    getArgument,
    NamedResolverFunction,
    RootResolverValue,
    ResultBuilder,
    nullRes,
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.MonadResolver
  ( MonadResolver,
    getArgument,
  )
import Data.Morpheus.App.Internal.Resolving.Resolver (Resolver)
import Data.Morpheus.App.Internal.Resolving.RootResolverValue (RootResolverValue (..))
import Data.Morpheus.App.Internal.Resolving.Types
  ( NamedResolver (..),
    NamedResolverRef (..),
    NamedResolverResult (..),
    ObjectTypeResolver (..),
    ResolverMap,
    ResolverValue (..),
    mkEnum,
    mkList,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    QUERY,
    TypeName,
    ValidValue,
  )

-- PUBLIC

-- fields
enum :: TypeName -> ResolverValue m
enum = mkEnum

list :: [ResolverValue m] -> ResolverValue m
list = mkList

ref :: (Applicative m) => TypeName -> ValidValue -> ResolverValue m
ref typeName = ResRef . pure . NamedResolverRef typeName . pure

refs :: (Applicative m) => TypeName -> [ValidValue] -> ResolverValue m
refs typeName = mkList . map (ref typeName)

type NamedResolverFunction o e m = NamedFunction (Resolver o e m)

type NamedFunction m = [ValidValue] -> m [ResultBuilder m]

-- types
object :: (MonadResolver m) => [(FieldName, m (ResolverValue m))] -> m (ResultBuilder m)
object = pure . Object

variant :: (MonadResolver m) => TypeName -> ValidValue -> m (ResultBuilder m)
variant tName = pure . Union tName

nullRes :: (MonadResolver m) => m (ResultBuilder m)
nullRes = pure Null

queryResolvers :: (Monad m) => [(TypeName, NamedFunction (Resolver QUERY e m))] -> RootResolverValue e m
queryResolvers = NamedResolversValue . mkResolverMap

-- INTERNAL
data ResultBuilder m
  = Object [(FieldName, m (ResolverValue m))]
  | Union TypeName ValidValue
  | Null

mkResolverMap :: (MonadResolver m) => [(TypeName, NamedFunction m)] -> ResolverMap m
mkResolverMap = HM.fromList . map packRes
  where
    packRes :: (MonadResolver m) => (TypeName, NamedFunction m) -> (TypeName, NamedResolver m)
    packRes (typeName, f) = (typeName, NamedResolver typeName (fmap (map mapValue) . f))
      where
        mapValue (Object x) = NamedObjectResolver (ObjectTypeResolver $ HM.fromList x)
        mapValue (Union name x) = NamedUnionResolver (NamedResolverRef name [x])
        mapValue Null = NamedNullResolver
