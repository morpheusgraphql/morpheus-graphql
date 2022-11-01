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
import Data.Morpheus.App.Internal.Resolving.Resolver (LiftOperation, Resolver, getArgument)
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

ref :: Applicative m => TypeName -> ValidValue -> ResolverValue m
ref typeName = ResRef . pure . NamedResolverRef typeName . pure

refs :: Applicative m => TypeName -> [ValidValue] -> ResolverValue m
refs typeName = mkList . map (ref typeName)

type NamedResolverFunction o e m = [ValidValue] -> Resolver o e m [ResultBuilder o e m]

-- types
object :: (LiftOperation o, Monad m) => [(FieldName, Resolver o e m (ResolverValue (Resolver o e m)))] -> Resolver o e m (ResultBuilder o e m)
object = pure . Object

variant :: (LiftOperation o, Monad m) => TypeName -> ValidValue -> Resolver o e m (ResultBuilder o e m)
variant tName = pure . Union tName

nullRes :: (LiftOperation o, Monad m) => Resolver o e m (ResultBuilder o e m)
nullRes = pure Null

queryResolvers :: Monad m => [(TypeName, NamedResolverFunction QUERY e m)] -> RootResolverValue e m
queryResolvers = NamedResolversValue . mkResolverMap

-- INTERNAL
data ResultBuilder o e m
  = Object [(FieldName, Resolver o e m (ResolverValue (Resolver o e m)))]
  | Union TypeName ValidValue
  | Null

mkResolverMap :: (LiftOperation o, Monad m) => [(TypeName, NamedResolverFunction o e m)] -> ResolverMap (Resolver o e m)
mkResolverMap = HM.fromList . map packRes
  where
    packRes :: (LiftOperation o, Monad m) => (TypeName, NamedResolverFunction o e m) -> (TypeName, NamedResolver (Resolver o e m))
    packRes (typeName, f) = (typeName, NamedResolver typeName (fmap (map mapValue) . f))
      where
        mapValue (Object x) = NamedObjectResolver (ObjectTypeResolver $ HM.fromList x)
        mapValue (Union name x) = NamedUnionResolver (NamedResolverRef name [x])
        mapValue Null = NamedNullResolver
