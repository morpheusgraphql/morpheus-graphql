{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Schema
  ( compileTimeSchemaValidation,
    deriveSchema,
    SCHEMA,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Map (alter, findWithDefault, insert)
import Data.Morpheus.Core (defaultConfig, validateSchema)
import Data.Morpheus.Generic (CBox, runCBox)
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Internal.Utils (IsMap (..), toAssoc)
import Data.Morpheus.Server.Deriving.Utils.GScan
  ( FreeCatType (..),
    scanFree,
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded (outputType)
import Data.Morpheus.Server.Deriving.Utils.Types (CatType (OutputType), GQLTypeNode (..), GQLTypeNodeExtension (..), NodeTypeVariant (..), coerceObject, fromSchema, nodeToType)
import Data.Morpheus.Server.Deriving.Utils.Use
  ( UseGQLType (useDeriveNode, useExploreRef, useFingerprint),
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    IgnoredResolver,
    ignoreUndefined,
    withGQL,
  )
import Data.Morpheus.Server.Types.TypeName (TypeFingerprint (..))
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    CONST,
    DirectiveDefinition,
    OBJECT,
    OUT,
    Schema,
    TypeCategory (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    defineDirective,
    defineSchemaWith,
    mkEnumContent,
    mkType,
    msg,
    toAny,
    unitTypeName,
  )
import Language.Haskell.TH (Exp, Q)
import Relude hiding (empty)

type SCHEMA qu mu su =
  ( GQLType (qu IgnoredResolver),
    GQLType (mu IgnoredResolver),
    GQLType (su IgnoredResolver)
  )

-- | normal morpheus server validates schema at runtime (after the schema derivation).
--   this method allows you to validate it at compile time.
compileTimeSchemaValidation :: (SCHEMA qu mu su) => proxy (root m event qu mu su) -> Q Exp
compileTimeSchemaValidation = fromSchema . (deriveSchema >=> validateSchema True defaultConfig)

explore :: forall f (a :: (Type -> Type) -> Type). (GQLType (a IgnoredResolver)) => f a -> [CBox FreeCatType GQLType]
explore _ = scanFree (useExploreRef withGQL) (OutputType :: CatType OUT (a IgnoredResolver))

toDerivation :: TypeFingerprint -> GQLTypeNode c -> (TypeFingerprint, GQLTypeNode ANY)
toDerivation fp (GQLTypeNode node xs) = (fp, GQLTypeNode (toAny node) xs)
toDerivation fp (GQLDirectiveNode node) = (fp, GQLDirectiveNode node)

resolveNode :: (GQLType a) => FreeCatType a -> GQLResult (TypeFingerprint, GQLTypeNode ANY)
resolveNode (FreeCatType proxy) = toDerivation (useFingerprint withGQL proxy) <$> useDeriveNode withGQL proxy

deriveRoot :: (GQLType a) => f a -> GQLResult (TypeDefinition OBJECT CONST)
deriveRoot prx = useDeriveNode withGQL (outputType prx) >>= nodeToType >>= coerceObject

data SchemaState where
  SchemaState ::
    { typeDefinitions :: Map TypeFingerprint (TypeDefinition ANY CONST),
      implements :: Map TypeName [TypeName],
      directiveDefinitions :: Map TypeFingerprint (DirectiveDefinition CONST)
    } ->
    SchemaState

instance Semigroup SchemaState where
  SchemaState t1 i1 d1 <> SchemaState t2 i2 d2 = SchemaState (t1 <> t2) (i1 <> i2) (d1 <> d2)

instance Monoid SchemaState where
  mempty = SchemaState mempty mempty mempty

insertImplements :: Map TypeName [TypeName] -> TypeDefinition c CONST -> TypeDefinition c CONST
insertImplements implementsMap TypeDefinition {typeContent = DataObject {..}, ..} = TypeDefinition {..}
  where
    typeContent = DataObject {objectImplements = objectImplements <> implements, ..}
    implements = findWithDefault [] typeName implementsMap
insertImplements _ t = t

checkTypeCollisions :: [(TypeFingerprint, TypeDefinition k a)] -> GQLResult [TypeDefinition k a]
checkTypeCollisions = fmap toList . foldlM collectTypes mempty
  where
    collectTypes :: Map (TypeName, TypeFingerprint) (TypeDefinition k a) -> (TypeFingerprint, TypeDefinition k a) -> GQLResult (Map (TypeName, TypeFingerprint) (TypeDefinition k a))
    collectTypes accum (fp, typ) = maybe addType (handleCollision typ) (key `lookup` accum)
      where
        addType = pure $ insert key typ accum
        key = (typeName typ, withSameCategory fp)
        handleCollision t1@TypeDefinition {typeContent = DataEnum {}} t2 | t1 == t2 = pure accum
        handleCollision TypeDefinition {typeContent = DataScalar {}} TypeDefinition {typeContent = DataScalar {}} = pure accum
        handleCollision TypeDefinition {typeName = name1} _ = failureRequirePrefix name1

failureRequirePrefix :: TypeName -> GQLResult b
failureRequirePrefix typename =
  throwError
    $ "It appears that the Haskell type "
    <> msg typename
    <> " was used as both input and output type, which is not allowed by GraphQL specifications."
    <> "\n\n "
    <> "If you use \"InputTypeNamespace\" directive, "
    <> "you can override the default type names for "
    <> msg typename
    <> " to solve this problem."

withSameCategory :: TypeFingerprint -> TypeFingerprint
withSameCategory (TypeableFingerprint _ xs) = TypeableFingerprint OUT xs
withSameCategory x = x

execNode :: (TypeFingerprint, GQLTypeNode ANY) -> SchemaState -> SchemaState
execNode (InternalFingerprint {}, _) s = s
execNode (fp, GQLTypeNode t xs) s = foldr execExtension (s {typeDefinitions = insert fp t (typeDefinitions s)}) xs
execNode (fp, GQLDirectiveNode d) s = s {directiveDefinitions = insert fp d (directiveDefinitions s)}

execExtension :: GQLTypeNodeExtension -> SchemaState -> SchemaState
execExtension (ImplementsExtension interface types) s = s {implements = foldr insertInterface (implements s) types}
  where
    insertInterface = alter (Just . (interface :) . fromMaybe [])
execExtension (UnionVariantsExtension nodes) s = foldr execNodeTypeVariant s nodes

execNodeTypeVariant :: NodeTypeVariant -> SchemaState -> SchemaState
execNodeTypeVariant (NodeTypeVariant consName fields) s = s {typeDefinitions = insert fp t (typeDefinitions s)}
  where
    fp = CustomFingerprint consName
    t = mkType consName fields
execNodeTypeVariant NodeUnitType s = s {typeDefinitions = insert fp t (typeDefinitions s)}
  where
    fp = CustomFingerprint unitTypeName
    t = mkType unitTypeName (mkEnumContent [unitTypeName])

deriveSchema :: forall root f m e qu mu su. (SCHEMA qu mu su) => f (root m e qu mu su) -> GQLResult (Schema CONST)
deriveSchema _ = do
  query <- deriveRoot (Proxy @(qu IgnoredResolver))
  mutation <- traverse deriveRoot (ignoreUndefined (Proxy @(mu IgnoredResolver)))
  subscription <- traverse deriveRoot (ignoreUndefined (Proxy @(su IgnoredResolver)))
  typeNodes <- traverse (runCBox resolveNode) (explore (Proxy @qu) <> explore (Proxy @mu) <> explore (Proxy @su))
  let SchemaState {..} = foldr execNode mempty typeNodes
  types <- map (insertImplements implements) <$> checkTypeCollisions (toAssoc typeDefinitions)
  schema <- defineSchemaWith types (Just query, mutation, subscription)
  foldlM defineDirective schema directiveDefinitions
