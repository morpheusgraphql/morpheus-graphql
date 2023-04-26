{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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
    NodeType (..),
  )
where

import Control.Monad.Except (MonadError (..))
import qualified Data.Map as Map
import Data.Morpheus.Internal.Ext (GQLResult)
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

data NodeType = TypeNodeUnion [NodeTypeVariant]

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
      Maybe (TypeDefinition OBJECT CONST)
    ) ->
  GQLResult (Schema CONST)
toSchema (SchemaBuilder v) = do
  ((q, m, s), typeDefs) <- v
  SchemaState {typeDefinitions, implements, directiveDefinitions} <- execUpdates mempty typeDefs
  types <- map (insertImplements implements) <$> checkTypeCollisions (Map.toList typeDefinitions)
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
    implements = Map.findWithDefault [] typeName x
insertImplements _ t = t

checkTypeCollisions :: [(TypeFingerprint, TypeDefinition k a)] -> GQLResult [TypeDefinition k a]
checkTypeCollisions = fmap Map.elems . foldlM collectTypes Map.empty
  where
    collectTypes :: Map (TypeName, TypeFingerprint) (TypeDefinition k a) -> (TypeFingerprint, TypeDefinition k a) -> GQLResult (Map (TypeName, TypeFingerprint) (TypeDefinition k a))
    collectTypes accum (fp, typ) = maybe addType (handleCollision typ) (key `Map.lookup` accum)
      where
        addType = pure $ Map.insert key typ accum
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

execUpdates :: Monad m => SchemaState -> [NodeDerivation] -> m SchemaState
execUpdates s = foldlM (&) s . map exec

exec :: Monad m => NodeDerivation -> SchemaState -> m SchemaState
exec (TypeDerivation InternalFingerprint {} _) s = pure s
exec (TypeDerivation fp t) s = pure s {typeDefinitions = Map.insert fp t (typeDefinitions s)}
exec (DirectiveDerivation InternalFingerprint {} _) s = pure s
exec (DirectiveDerivation fp d) s = pure s {directiveDefinitions = Map.insert fp d (directiveDefinitions s)}
exec (ImplementsDerivation interface types) s = pure $ s {implements = foldr insertInterface (implements s) types}
  where
    insertInterface = Map.alter (Just . (interface :) . fromMaybe [])
exec (UnionType nodes) s = foldlM (&) s (map execNode nodes)

execNode :: Monad m => NodeTypeVariant -> SchemaState -> m SchemaState
execNode (NodeTypeVariant consName fields) s =
  pure s {typeDefinitions = Map.insert fp t (typeDefinitions s)}
  where
    fp = CustomFingerprint consName
    t = mkType consName fields
execNode NodeUnitType s = pure s {typeDefinitions = Map.insert fp t (typeDefinitions s)}
  where
    fp = CustomFingerprint unitTypeName
    t = mkType unitTypeName (mkEnumContent [unitTypeName])

derivations :: [NodeDerivation] -> SchemaBuilder ()
derivations nodes = SchemaBuilder $ pure ((), nodes)