{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.SchemaBuilder
  ( SchemaBuilder (..),
    TypeFingerprint (..),
    toSchema,
    NodeDerivation (..),
    derivations,
    NodeTypeVariant (..),
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Map (alter, findWithDefault, insert)
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Internal.Utils (IsMap (..))
import Data.Morpheus.Server.Types.TypeName (TypeFingerprint (..))
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    CONST,
    DirectiveDefinition,
    GQLError,
    OBJECT,
    Schema,
    TRUE,
    TypeCategory (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    defineDirective,
    defineSchemaWith,
    mkEnumContent,
    mkType,
    msg,
    unitTypeName,
  )
import Relude hiding (empty)

data NodeTypeVariant
  = NodeTypeVariant TypeName (TypeContent TRUE ANY CONST)
  | NodeUnitType

data NodeDerivation
  = TypeDerivation TypeFingerprint (TypeDefinition ANY CONST)
  | DirectiveDerivation TypeFingerprint (DirectiveDefinition CONST)
  | ImplementsDerivation TypeName [TypeName]
  | UnionType [NodeTypeVariant]

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

-- Helper Functions
newtype SchemaBuilder a = SchemaBuilder
  { runSchemaT :: GQLResult (a, [NodeDerivation])
  }
  deriving (Functor)

instance MonadError GQLError SchemaBuilder where
  throwError = SchemaBuilder . throwError
  catchError (SchemaBuilder mx) f = SchemaBuilder (catchError mx (runSchemaT . f))

instance Applicative SchemaBuilder where
  pure = SchemaBuilder . pure . (,[])
  (SchemaBuilder v1) <*> (SchemaBuilder v2) = SchemaBuilder $ do
    (f, u1) <- v1
    (a, u2) <- v2
    pure (f a, u1 <> u2)

instance Monad SchemaBuilder where
  return = pure
  (SchemaBuilder v1) >>= f =
    SchemaBuilder $ do
      (x, up1) <- v1
      (y, up2) <- runSchemaT (f x)
      pure (y, up1 <> up2)

toSchema ::
  SchemaBuilder
    ( TypeDefinition OBJECT CONST,
      Maybe (TypeDefinition OBJECT CONST),
      Maybe (TypeDefinition OBJECT CONST),
      [NodeDerivation]
    ) ->
  GQLResult (Schema CONST)
toSchema (SchemaBuilder v) = do
  ((q, m, s, nodes), typeDefs) <- v
  SchemaState {typeDefinitions, implements, directiveDefinitions} <- foldlM (&) mempty (map execNode (typeDefs <> nodes))
  types <- map (insertImplements implements) <$> checkTypeCollisions (toAssoc typeDefinitions)
  schema <- defineSchemaWith types (Just q, m, s)
  foldlM defineDirective schema directiveDefinitions

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
execNode (ImplementsDerivation interface types) s = pure $ s {implements = foldr insertInterface (implements s) types}
  where
    insertInterface = alter (Just . (interface :) . fromMaybe [])
execNode (UnionType nodes) s = foldlM (&) s (map execNodeTypeVariant nodes)

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

derivations :: [NodeDerivation] -> SchemaBuilder ()
derivations nodes = SchemaBuilder $ pure ((), nodes)
