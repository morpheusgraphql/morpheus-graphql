{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.SchemaBuilder
  ( TypeFingerprint (..),
    NodeDerivation (..),
    SchemaState (..),
    insertImplements,
    execNode,
    checkTypeCollisions,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Map (alter, findWithDefault, insert)
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Internal.Utils (IsMap (..))
import Data.Morpheus.Server.Deriving.Utils.Types (GQLTypeNodeExtension (..), NodeTypeVariant (..))
import Data.Morpheus.Server.Types.TypeName (TypeFingerprint (..))
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    CONST,
    DirectiveDefinition,
    TypeCategory (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    mkEnumContent,
    mkType,
    msg,
    unitTypeName,
  )
import Relude hiding (empty)

data NodeDerivation
  = TypeDerivation TypeFingerprint (TypeDefinition ANY CONST)
  | DirectiveDerivation TypeFingerprint (DirectiveDefinition CONST)
  | NodeExtension GQLTypeNodeExtension

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
insertImplements x TypeDefinition {typeContent = DataObject {..}, ..} =
  TypeDefinition
    { typeContent = DataObject {objectImplements = objectImplements <> implements, ..},
      ..
    }
  where
    implements :: [TypeName]
    implements = findWithDefault [] typeName x
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
  throwError $
    "It appears that the Haskell type "
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

execNode :: Monad m => NodeDerivation -> SchemaState -> m SchemaState
execNode (TypeDerivation InternalFingerprint {} _) s = pure s
execNode (TypeDerivation fp t) s = pure s {typeDefinitions = insert fp t (typeDefinitions s)}
execNode (DirectiveDerivation InternalFingerprint {} _) s = pure s
execNode (DirectiveDerivation fp d) s = pure s {directiveDefinitions = insert fp d (directiveDefinitions s)}
execNode (NodeExtension (ImplementsExtension interface types)) s = pure $ s {implements = foldr insertInterface (implements s) types}
  where
    insertInterface = alter (Just . (interface :) . fromMaybe [])
execNode (NodeExtension (UnionVariantsExtension nodes)) s = foldlM (&) s (map execNodeTypeVariant nodes)

execNodeTypeVariant :: Monad m => NodeTypeVariant -> SchemaState -> m SchemaState
execNodeTypeVariant (NodeTypeVariant consName fields) s =
  pure s {typeDefinitions = insert fp t (typeDefinitions s)}
  where
    fp = CustomFingerprint consName
    t = mkType consName fields
execNodeTypeVariant NodeUnitType s = pure s {typeDefinitions = insert fp t (typeDefinitions s)}
  where
    fp = CustomFingerprint unitTypeName
    t = mkType unitTypeName (mkEnumContent [unitTypeName])
