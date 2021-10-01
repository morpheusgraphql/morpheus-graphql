{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Utils
  ( withObject,
    ResolverValueDefinition (..),
    mkEnum,
    mkBoolean,
    mkFloat,
    mkInt,
    mkList,
    mkNull,
    mkString,
    unpackJSONName,
    resolveResolverDefinition,
    resolveObjectTypeResolver,
    ObjectTypeResolver (..),
    requireObject,
    NamedResolverRef (..),
    ResolverObject,
    ResolverValue,
    FieldValue (..),
    mkObject,
    mkObject',
    lookupResJSON,
    resolveObject,
    mkValue,
    mkUnion,
    ResolverEntry,
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
  )
import Data.Morpheus.Error (subfieldsNotSelected)
import Data.Morpheus.Internal.Ext ((<:>), Merge (..))
import Data.Morpheus.Internal.Utils (KeyOf (keyOf), selectOr, traverseCollection)
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    FieldName,
    GQLError,
    GQLError,
    ObjectEntry (..),
    ScalarValue (..),
    ScalarValue (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    SelectionSet,
    TypeName,
    TypeName,
    UnionTag (..),
    VALID,
    VALID,
    ValidValue,
    ValidValue,
    Value (..),
    decodeScientific,
    internal,
    packName,
    packName,
    unitFieldName,
    unitTypeName,
    unpackName,
  )
import qualified Data.Vector as V
import GHC.Show (Show (show))
import Relude

mkObject ::
  TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkObject __typename = ResObject Nothing . mkObject' __typename

lookupResJSON :: (MonadError GQLError f, Monad m) => Text -> A.Value -> f (ResolverObject m)
lookupResJSON name (A.Object fields) =
  selectOr
    (mkEmptyObject name)
    (requireObject . mkValue)
    name
    fields
lookupResJSON name _ = mkEmptyObject name

mkEmptyObject :: Monad m => Text -> m (ResolverObject a)
mkEmptyObject name = pure $ ObjectTypeResolver (packName name) mempty

mkObject' ::
  TypeName ->
  [(FieldName, m (ResolverValue m))] ->
  ObjectTypeResolver (FieldValue m)
mkObject' __typename fields =
  ( ObjectTypeResolver
      { __typename,
        objectFields = HM.fromList (map (second FieldValue) fields)
      }
  )

mkValue ::
  (Monad m) =>
  A.Value ->
  ResolverValue m
mkValue (A.Object v) =
  mkObject
    (maybe "__JSON__" unpackJSONName $ HM.lookup "__typename" v)
    $ fmap
      (bimap packName (pure . mkValue))
      (HM.toList v)
mkValue (A.Array ls) = mkList (fmap mkValue (V.toList ls))
mkValue A.Null = mkNull
mkValue (A.Number x) = ResScalar (decodeScientific x)
mkValue (A.String x) = ResScalar (String x)
mkValue (A.Bool x) = ResScalar (Boolean x)

mkUnion ::
  (Monad m) =>
  TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkUnion name fields =
  ResObject
    (Just name)
    $ ObjectTypeResolver
      { __typename = name,
        objectFields = HM.fromList (map (second FieldValue) fields)
      }

data ObjectTypeResolver a = ObjectTypeResolver
  { __typename :: TypeName,
    objectFields :: HashMap FieldName a
  }
  deriving (Show)

data NamedResolverRef = NamedResolverRef
  { resolverTypeName :: TypeName,
    resolverArgument :: ValidValue
  }
  deriving (Show)

resolveObjectTypeResolver ::
  ( MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  (a -> m ValidValue) ->
  ObjectTypeResolver a ->
  SelectionSet VALID ->
  m ValidValue
resolveObjectTypeResolver f drv@ObjectTypeResolver {__typename} =
  fmap Object . traverseCollection resolver
  where
    resolver currentSelection =
      local (\ctx -> ctx {currentSelection, currentTypeName = __typename}) $
        ObjectEntry (keyOf currentSelection)
          <$> runFieldResolver
            f
            currentSelection
            drv

runFieldResolver ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  (a -> m ValidValue) ->
  Selection VALID ->
  ObjectTypeResolver a ->
  m ValidValue
runFieldResolver f Selection {selectionName}
  | selectionName == "__typename" =
    pure . Scalar . String . unpackName . __typename
  | otherwise =
    maybe (pure Null) f
      . HM.lookup selectionName
      . objectFields

withObject ::
  ( Monad m,
    MonadError GQLError m
  ) =>
  TypeName ->
  (SelectionSet VALID -> m value) ->
  Selection VALID ->
  m value
withObject __typename f Selection {selectionName, selectionContent, selectionPosition} = checkContent selectionContent
  where
    checkContent (SelectionSet selection) = f selection
    checkContent (UnionSelection interface unionSel) = do
      selection <- selectOr (pure interface) ((interface <:>) . unionTagSelection) __typename unionSel
      f selection
    checkContent _ = throwError $ subfieldsNotSelected selectionName "" selectionPosition

instance (MonadError GQLError m) => Semigroup (FieldValue m) where
  FieldValue a <> FieldValue b = FieldValue $ (,) <$> a <*> b >>= uncurry merge

instance
  ( Monad m,
    Applicative f,
    MonadError GQLError m
  ) =>
  Merge f (ResolverObject m)
  where
  merge (ObjectTypeResolver typeName x) (ObjectTypeResolver _ y) =
    pure $ ObjectTypeResolver typeName (HM.unionWith (<>) x y)

newtype FieldValue m = FieldValue {unFieldValue :: m (ResolverValue m)}

type ResolverValue = ResolverValueDefinition

type ResolverObject m = ObjectTypeResolver (FieldValue m)

data ResolverValueDefinition (m :: Type -> Type)
  = ResNull
  | ResScalar ScalarValue
  | ResList [ResolverValueDefinition m]
  | ResEnum TypeName
  | ResObject (Maybe TypeName) (ResolverObject m)
  | ResRef (m NamedResolverRef)

instance Show (ResolverValueDefinition m) where
  show = undefined

instance IsString (ResolverValueDefinition m) where
  fromString = ResScalar . fromString

requireObject :: MonadError GQLError f => ResolverValueDefinition m -> f (ResolverObject m)
requireObject (ResObject _ x) = pure x
requireObject _ = throwError (internal "resolver must be an object")

instance
  ( Monad f,
    MonadError GQLError f,
    Merge f (ResolverObject m)
  ) =>
  Merge f (ResolverValueDefinition m)
  where
  merge ResNull ResNull = pure ResNull
  merge ResScalar {} x@ResScalar {} = pure x
  merge ResEnum {} x@ResEnum {} = pure x
  merge (ResObject n x) (ResObject _ y) = ResObject n <$> merge x y
  merge _ _ = throwError (internal "can't merge: incompatible resolvers")

mkString :: Text -> ResolverValueDefinition m
mkString = ResScalar . String

mkFloat :: Double -> ResolverValueDefinition m
mkFloat = ResScalar . Float

mkInt :: Int -> ResolverValueDefinition m
mkInt = ResScalar . Int

mkBoolean :: Bool -> ResolverValueDefinition m
mkBoolean = ResScalar . Boolean

mkList :: [ResolverValueDefinition m] -> ResolverValueDefinition m
mkList = ResList

mkNull :: ResolverValueDefinition m
mkNull = ResNull

unpackJSONName :: A.Value -> TypeName
unpackJSONName (A.String x) = packName x
unpackJSONName _ = "__JSON__"

mkEnum :: TypeName -> ResolverValueDefinition m
mkEnum = ResEnum

resolveResolverDefinition ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  ResolverValueDefinition m ->
  m ValidValue
resolveResolverDefinition res =
  asks currentSelection
    >>= encodeResolverDefinition
      res
      . selectionContent

resolveObject ::
  ( MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  ObjectTypeResolver (FieldValue m) ->
  SelectionSet VALID ->
  m ValidValue
resolveObject = resolveObjectTypeResolver (unFieldValue >=> resolveResolverDefinition)

type ResolverEntry m = (FieldName, m (ResolverValue m))

mkEnumNull :: (Monad m) => [ResolverEntry m]
mkEnumNull = [(unitFieldName, pure $ mkEnum unitTypeName)]

-- LIST
encodeResolverDefinition ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  -- EncoderContext m ->
  ResolverValueDefinition m ->
  SelectionContent VALID ->
  m ValidValue
encodeResolverDefinition (ResList xs) selection =
  List <$> traverse (`encodeResolverDefinition` selection) xs
-- Object -----------------
encodeResolverDefinition (ResObject tyName obj) _ = do
  currentTypeName <- asks currentTypeName
  asks currentSelection >>= withObject (fromMaybe currentTypeName tyName) (resolveObject obj)
-- ENUM
encodeResolverDefinition (ResEnum enum) SelectionField = handle enum
  where
    handle x = pure $ Scalar $ String $ unpackName x
encodeResolverDefinition (ResEnum name) unionSel@UnionSelection {} =
  encodeResolverDefinition (mkUnion name mkEnumNull) unionSel
encodeResolverDefinition ResEnum {} _ = throwError (internal "wrong selection on enum value")
-- SCALARS
encodeResolverDefinition ResNull _ = pure Null
encodeResolverDefinition (ResScalar x) SelectionField = pure $ Scalar x
encodeResolverDefinition ResScalar {} _ =
  throwError (internal "scalar Resolver should only receive SelectionField")
encodeResolverDefinition (ResRef x) _ = undefined
