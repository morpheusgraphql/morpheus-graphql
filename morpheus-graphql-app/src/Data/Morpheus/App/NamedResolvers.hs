module Data.Morpheus.App.NamedResolvers (
    ref,
    object,
    variant,
    list,
    refs,
    enum,
    queryResolvers,
    getArgument,
    NamedResolverFunction,
    RootResolverValue
) where

import qualified Data.HashMap.Internal as HM

import Data.Morpheus.App.Internal.Resolving.NamedResolver
    ( NamedResolverRef (..),
      NamedResolverField,
      NamedResolverResult(..),
      NamedResolver(..),
      ResolverMap 
    )
import Data.Morpheus.Types.Internal.AST
    ( TypeName,
      QUERY,
      FieldName,
      TypeName,
      TypeName,
      ValidValue 
    )
import Data.Morpheus.App.Internal.Resolving.RootResolverValue ( RootResolverValue(..) )
import Data.Morpheus.App.Internal.Resolving.Resolver (getArgument,Resolver, LiftOperation)
import Data.Morpheus.App.Internal.Resolving.Utils
  ( ObjectTypeResolver (..),
    ResolverValueDefinition (..),
    mkList, mkEnum
  )

-- PUBLIC
queryResolvers :: Monad m =>[(TypeName, NamedResolverFunction QUERY  e m)] -> RootResolverValue e m
queryResolvers = NamedResolvers . mkResolverMap

type NamedResolverFunction o e m = ValidValue -> ResultBuilder o e m

enum :: TypeName -> NamedResolverField
enum = mkEnum 

list :: [NamedResolverField ] -> NamedResolverField
list = mkList

object :: [(FieldName, Resolver o e m NamedResolverField)] -> ResultBuilder  o e m
object = Object

variant ::  TypeName -> ValidValue -> ResultBuilder o e m
variant  = Union

ref :: TypeName -> ValidValue -> NamedResolverField
ref typeName = ResObject . NamedResolverRef typeName

refs :: TypeName -> [ValidValue] -> NamedResolverField
refs typeName = mkList . map (ref typeName)

-- INTERNAL
data ResultBuilder o e m =
   Object [(FieldName, Resolver o e m NamedResolverField)]
  | Union  TypeName  ValidValue

mkResolverMap :: (LiftOperation o, Monad m) => [(TypeName, NamedResolverFunction o e m)] -> ResolverMap (Resolver o e m)
mkResolverMap = HM.fromList . map packRes
  where
    packRes :: (LiftOperation o, Monad m) => (TypeName,ValidValue -> ResultBuilder o e m) -> (TypeName, NamedResolver (Resolver o e m))
    packRes (typeName, value) =
      ( typeName,
        NamedResolver
          typeName
          ( pure
              . mapValue
              . value
          )
      )
      where
        mapValue (Object x)= NamedObjectResolver (ObjectTypeResolver typeName $ HM.fromList x)
        mapValue (Union name x)= NamedUnionResolver (NamedResolverRef name x)
