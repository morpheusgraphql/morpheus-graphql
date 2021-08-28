{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
    EncoderContext (..),
    ObjectTypeResolver (..),
    requireObject,
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as A
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
  )
import Data.Morpheus.Error (subfieldsNotSelected)
import Data.Morpheus.Internal.Ext (Merge (..), (<:>))
import Data.Morpheus.Internal.Utils (KeyOf (keyOf), selectOr, traverseCollection)
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    GQLError,
    Msg (msg),
    ObjectEntry (..),
    ScalarValue (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeName,
    UnionTag (..),
    VALID,
    ValidValue,
    Value (..),
    internal,
    packName,
    unpackName,
  )
import Relude

data ObjectTypeResolver a = ObjectTypeResolver
  { __typename :: TypeName,
    objectFields :: HashMap FieldName a
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
    checkContent (UnionSelection interfaceSel unionSel) =
      f (selectOr interfaceSel unionTagSelection __typename unionSel)
    checkContent _ = throwError $ subfieldsNotSelected selectionName "" selectionPosition

data ResolverValueDefinition u o
  = ResNull
  | ResScalar ScalarValue
  | ResEnum TypeName
  | ResList [ResolverValueDefinition u o]
  | ResUnion TypeName u
  | ResObject o
  deriving (Show)

instance IsString (ResolverValueDefinition u o) where
  fromString = ResScalar . fromString

requireObject :: MonadError GQLError m => ResolverValueDefinition u a -> m a
requireObject (ResObject x) = pure x
requireObject _ = throwError (internal "resolver must be an object")

instance
  ( Monad f,
    MonadError GQLError f,
    Merge f o
  ) =>
  Merge f (ResolverValueDefinition u o)
  where
  merge ResNull ResNull = pure ResNull
  merge ResScalar {} x@ResScalar {} = pure x
  merge ResEnum {} x@ResEnum {} = pure x
  merge (ResObject x) (ResObject y) = ResObject <$> merge x y
  merge _ _ = throwError (internal "can't merge: incompatible resolvers")

mkString :: Text -> ResolverValueDefinition u o
mkString = ResScalar . String

mkFloat :: Double -> ResolverValueDefinition u o
mkFloat = ResScalar . Float

mkInt :: Int -> ResolverValueDefinition u o
mkInt = ResScalar . Int

mkBoolean :: Bool -> ResolverValueDefinition u o
mkBoolean = ResScalar . Boolean

mkList :: [ResolverValueDefinition u o] -> ResolverValueDefinition u o
mkList = ResList

mkNull :: ResolverValueDefinition u o
mkNull = ResNull

unpackJSONName :: A.Value -> TypeName
unpackJSONName (A.String x) = packName x
unpackJSONName _ = "__JSON__"

mkEnum :: TypeName -> ResolverValueDefinition u o
mkEnum = ResEnum

data EncoderContext u o m = EncoderContext
  { encodeObject :: o -> SelectionSet VALID -> m ValidValue,
    encodeUnion :: u -> SelectionSet VALID -> m ValidValue,
    getTypeName :: o -> TypeName,
    mkEnumUnion :: TypeName -> ResolverValueDefinition u o
  }

resolveResolverDefinition ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  EncoderContext u o m ->
  ResolverValueDefinition u o ->
  m ValidValue
resolveResolverDefinition ctx res =
  asks currentSelection
    >>= encodeResolverDefinition
      ctx
      res
      . selectionContent

-- LIST
encodeResolverDefinition ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  EncoderContext u o m ->
  ResolverValueDefinition u o ->
  SelectionContent VALID ->
  m ValidValue
encodeResolverDefinition ctx (ResList xs) selection =
  List <$> traverse (\res -> encodeResolverDefinition ctx res selection) xs
-- Object -----------------
encodeResolverDefinition EncoderContext {getTypeName, encodeObject} (ResObject obj) _ =
  asks currentSelection >>= withObject (getTypeName obj) (encodeObject obj)
-- ENUM
encodeResolverDefinition _ (ResEnum enum) SelectionField = pure $ Scalar $ String $ unpackName enum
encodeResolverDefinition ctx@EncoderContext {mkEnumUnion} (ResEnum name) unionSel@UnionSelection {} =
  encodeResolverDefinition ctx (mkEnumUnion name) unionSel
encodeResolverDefinition _ ResEnum {} _ = throwError (internal "wrong selection on enum value")
-- UNION
encodeResolverDefinition EncoderContext {encodeUnion} (ResUnion typename unionRes) (UnionSelection interface selections) = do
  selection <- selectOr (pure interface) ((interface <:>) . unionTagSelection) typename selections
  encodeUnion unionRes selection
encodeResolverDefinition EncoderContext {encodeUnion} (ResUnion _ unionRes) (SelectionSet selection) =
  encodeUnion unionRes selection
encodeResolverDefinition _ (ResUnion name _) SelectionField =
  throwError (internal $ "union Resolver " <> msg name <> " cant resolve  SelectionField")
-- SCALARS
encodeResolverDefinition _ ResNull _ = pure Null
encodeResolverDefinition _ (ResScalar x) SelectionField = pure $ Scalar x
encodeResolverDefinition _ ResScalar {} _ =
  throwError (internal "scalar Resolver should only receive SelectionField")
