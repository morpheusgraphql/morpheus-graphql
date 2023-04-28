{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.Types
  ( argumentsToObject,
    nodeToType,
    GQLTypeNode (..),
    DerivingMonad,
    fromSchema,
    withObject,
    typeToArguments,
    CatType (..),
    NodeTypeVariant (..),
    GQLTypeNodeExtension (..),
  )
where

import Control.Monad.Except (MonadError (..), throwError)
import Data.Morpheus.Internal.Ext
  ( GQLResult,
    Result (Failure, Success, errors),
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    Argument (..),
    Arguments,
    ArgumentsDefinition,
    CONST,
    DirectiveDefinition (..),
    FieldsDefinition,
    GQLError,
    IN,
    Msg (..),
    OUT,
    ObjectEntry (..),
    Schema (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    VALID,
    Value (..),
    fieldsToArguments,
  )
import Language.Haskell.TH (Exp, Q)
import Relude hiding (empty)

type DerivingMonad m = (MonadError GQLError m)

fromSchema :: GQLResult (Schema VALID) -> Q Exp
fromSchema Success {} = [|()|]
fromSchema Failure {errors} = fail (show errors)

withObject :: (DerivingMonad m) => TypeName -> TypeContent TRUE any s -> m (FieldsDefinition OUT s)
withObject _ DataObject {objectFields} = pure objectFields
withObject name _ = failureOnlyObject name

failureOnlyObject :: (DerivingMonad m) => TypeName -> m b
failureOnlyObject name = throwError $ msg name <> " should have only one nonempty constructor"

typeToArguments :: (DerivingMonad m) => TypeDefinition IN CONST -> m (ArgumentsDefinition CONST)
typeToArguments TypeDefinition {typeContent = DataInputObject {inputObjectFields}} = pure $ fieldsToArguments inputObjectFields
typeToArguments TypeDefinition {typeName} = failureOnlyObject typeName

argumentsToObject :: Arguments VALID -> Value VALID
argumentsToObject = Object . fmap toEntry
  where
    toEntry Argument {..} = ObjectEntry argumentName argumentValue

data GQLTypeNodeExtension
  = ImplementsExtension TypeName [TypeName]
  | UnionVariantsExtension [NodeTypeVariant]

data GQLTypeNode c
  = GQLTypeNode (TypeDefinition c CONST) [GQLTypeNodeExtension]
  | GQLDirectiveNode (DirectiveDefinition CONST)

nodeToType :: (Applicative f, MonadError GQLError f) => GQLTypeNode c -> f (TypeDefinition c CONST)
nodeToType node = case node of
  GQLTypeNode x _ -> pure x
  GQLDirectiveNode dir -> throwError $ "expected " <> msg (directiveDefinitionName dir) <> " to be a type but its directive!"

data NodeTypeVariant
  = NodeTypeVariant TypeName (TypeContent TRUE ANY CONST)
  | NodeUnitType
