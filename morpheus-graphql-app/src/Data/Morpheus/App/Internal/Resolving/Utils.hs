{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
import Data.Morpheus.Internal.Ext ((<:>), Merge (..))
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
import GHC.Show (Show (show))
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

data ResolverValueDefinition o (m :: Type -> Type)
  = ResNull
  | ResScalar ScalarValue
  | ResEnum TypeName
  | ResList [ResolverValueDefinition o m]
  | ResObject TypeName o
  | ResUnion TypeName (m o)

instance Show (ResolverValueDefinition o m) where
  show = undefined

instance IsString (ResolverValueDefinition o m) where
  fromString = ResScalar . fromString

requireObject :: MonadError GQLError f => ResolverValueDefinition o m -> f o
requireObject (ResObject _ x) = pure x
requireObject _ = throwError (internal "resolver must be an object")

instance
  ( Monad f,
    MonadError GQLError f,
    Merge f o
  ) =>
  Merge f (ResolverValueDefinition o m)
  where
  merge ResNull ResNull = pure ResNull
  merge ResScalar {} x@ResScalar {} = pure x
  merge ResEnum {} x@ResEnum {} = pure x
  merge (ResObject n x) (ResObject _ y) = ResObject n <$> merge x y
  merge _ _ = throwError (internal "can't merge: incompatible resolvers")

mkString :: Text -> ResolverValueDefinition o m
mkString = ResScalar . String

mkFloat :: Double -> ResolverValueDefinition o m
mkFloat = ResScalar . Float

mkInt :: Int -> ResolverValueDefinition o m
mkInt = ResScalar . Int

mkBoolean :: Bool -> ResolverValueDefinition o m
mkBoolean = ResScalar . Boolean

mkList :: [ResolverValueDefinition o m] -> ResolverValueDefinition o m
mkList = ResList

mkNull :: ResolverValueDefinition o m
mkNull = ResNull

unpackJSONName :: A.Value -> TypeName
unpackJSONName (A.String x) = packName x
unpackJSONName _ = "__JSON__"

mkEnum :: TypeName -> ResolverValueDefinition o m
mkEnum = ResEnum

data EncoderContext o m = EncoderContext
  { encodeNode :: o -> SelectionSet VALID -> m ValidValue,
    mkEnumUnion :: TypeName -> ResolverValueDefinition o m
  }

resolveResolverDefinition ::
  ( Monad m,
    MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  EncoderContext o m ->
  ResolverValueDefinition o m ->
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
  EncoderContext o m ->
  ResolverValueDefinition o m ->
  SelectionContent VALID ->
  m ValidValue
encodeResolverDefinition ctx (ResList xs) selection =
  List <$> traverse (\res -> encodeResolverDefinition ctx res selection) xs
-- Object -----------------
encodeResolverDefinition EncoderContext {encodeNode} (ResObject typeName obj) _ =
  asks currentSelection >>= withObject typeName (encodeNode obj)
-- ENUM
encodeResolverDefinition _ (ResEnum enum) SelectionField = pure $ Scalar $ String $ unpackName enum
encodeResolverDefinition ctx@EncoderContext {mkEnumUnion} (ResEnum name) unionSel@UnionSelection {} =
  encodeResolverDefinition ctx (mkEnumUnion name) unionSel
encodeResolverDefinition _ ResEnum {} _ = throwError (internal "wrong selection on enum value")
-- UNION
encodeResolverDefinition EncoderContext {encodeNode} (ResUnion typename unionRes) (UnionSelection interface selections) = do
  selection <- selectOr (pure interface) ((interface <:>) . unionTagSelection) typename selections
  unionRes >>= (`encodeNode` selection)
encodeResolverDefinition EncoderContext {encodeNode} (ResUnion _ unionRes) (SelectionSet selection) =
  unionRes >>= (`encodeNode` selection)
encodeResolverDefinition _ (ResUnion name _) SelectionField =
  throwError (internal $ "union Resolver " <> msg name <> " cant resolve  SelectionField")
-- SCALARS
encodeResolverDefinition _ ResNull _ = pure Null
encodeResolverDefinition _ (ResScalar x) SelectionField = pure $ Scalar x
encodeResolverDefinition _ ResScalar {} _ =
  throwError (internal "scalar Resolver should only receive SelectionField")
