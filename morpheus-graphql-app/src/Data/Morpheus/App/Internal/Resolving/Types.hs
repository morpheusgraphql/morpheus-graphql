{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
    buildBatches,
    Cache,
    CacheKey (..),
    BatchEntry (..),
    LocalCache,
    dumpCache,
    useCached,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.HashMap.Internal.Strict as Hm
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.Core (RenderGQL, render)
import Data.Morpheus.Internal.Ext (Merge (..))
import Data.Morpheus.Internal.Utils (KeyOf (keyOf))
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    ScalarValue (..),
    SelectionContent,
    TypeName,
    VALID,
    ValidValue,
    internal,
  )
import GHC.Show (Show (show))
import Relude hiding (show)

type LocalCache = HashMap CacheKey ValidValue

useCached :: (Eq k, Hashable k, MonadError GQLError f) => HashMap k a -> k -> f a
useCached mp v = case HM.lookup v mp of
  Just x -> pure x
  Nothing -> throwError (internal "TODO:")

dumpCache :: Bool -> (LocalCache, ResolverMap m) -> (LocalCache, ResolverMap m)
dumpCache enabled xs
  | null (fst xs) || not enabled = xs
  | otherwise = trace ("\nCACHE:\n" <> intercalate "\n" (map printKeyValue $ HM.toList $ fst xs) <> "\n") xs
  where
    printKeyValue (key, v) = " " <> show key <> ": " <> unpack (render v)

printSel :: RenderGQL a => a -> [Char]
printSel sel = map replace $ filter ignoreSpaces $ unpack (render sel)
  where
    ignoreSpaces x = x /= ' '
    replace '\n' = ' '
    replace x = x

data BatchEntry = BatchEntry
  { batchedSelection :: SelectionContent VALID,
    batchedType :: TypeName,
    batchedArguments :: [ValidValue]
  }

instance Show BatchEntry where
  show (BatchEntry sel typename dep) = printSel sel <> ":" <> toString typename <> ":" <> show (map (unpack . render) dep)

data CacheKey = CacheKey
  { cachedSel :: SelectionContent VALID,
    cachedTypeName :: TypeName,
    cachedArg :: ValidValue
  }
  deriving (Eq, Generic)

instance Show CacheKey where
  show (CacheKey sel typename dep) = printSel sel <> ":" <> toString typename <> ":" <> unpack (render dep)

instance Hashable CacheKey where
  hashWithSalt s (CacheKey sel tyName arg) = hashWithSalt s (sel, tyName, render arg)

type Cache m = HashMap CacheKey (NamedResolverResult m)

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

uniq :: (Eq a, Hashable a) => [a] -> [a]
uniq = Hm.keys . HM.fromList . map (,True)

buildBatches :: [(SelectionContent VALID, NamedResolverRef)] -> [BatchEntry]
buildBatches inputs =
  let entityTypes = uniq $ map (second resolverTypeName) inputs
   in mapMaybe (selectByEntity inputs) entityTypes

selectByEntity :: [(SelectionContent VALID, NamedResolverRef)] -> (SelectionContent VALID, TypeName) -> Maybe BatchEntry
selectByEntity inputs (tSel, tName) = case filter areEq inputs of
  [] -> Nothing
  xs -> Just $ BatchEntry tSel tName (uniq $ concatMap (resolverArgument . snd) xs)
  where
    areEq (sel, v) = sel == tSel && tName == resolverTypeName v

data NamedResolverResult (m :: Type -> Type)
  = NamedObjectResolver (ObjectTypeResolver m)
  | NamedUnionResolver NamedResolverRef
  | NamedEnumResolver TypeName
  | NamedResolverNull

instance KeyOf TypeName (NamedResolver m) where
  keyOf = resolverName

instance Show (NamedResolverResult m) where
  show NamedObjectResolver {} = "NamedObjectResolver"
  show NamedUnionResolver {} = "NamedUnionResolver"
  show NamedEnumResolver {} = "NamedEnumResolver"
  show NamedResolverNull {} = "NamedResolverNull"

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
