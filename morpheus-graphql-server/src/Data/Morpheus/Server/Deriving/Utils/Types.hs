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
  )
where

import Control.Monad.Except (MonadError (..), throwError)
import Data.Morpheus.Internal.Ext
  ( GQLResult,
    Result (Failure, Success, errors),
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
    inputType,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    ArgumentsDefinition,
    CONST,
    DirectiveDefinition (..),
    FieldsDefinition,
    GQLError,
    IN,
    Msg (..),
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

withObject :: (DerivingMonad m) => TypeName -> CatType c a -> TypeContent TRUE any s -> m (FieldsDefinition c s)
withObject _ InputType DataInputObject {inputObjectFields} = pure inputObjectFields
withObject _ OutputType DataObject {objectFields} = pure objectFields
withObject name _ _ = failureOnlyObject name

failureOnlyObject :: (DerivingMonad m) => TypeName -> m b
failureOnlyObject name = throwError $ msg name <> " should have only one nonempty constructor"

typeToArguments :: (DerivingMonad m) => TypeDefinition IN CONST -> m (ArgumentsDefinition CONST)
typeToArguments TypeDefinition {..} = fieldsToArguments <$> withObject typeName (inputType (Proxy @())) typeContent

argumentsToObject :: Arguments VALID -> Value VALID
argumentsToObject = Object . fmap toEntry
  where
    toEntry Argument {..} = ObjectEntry argumentName argumentValue

data GQLTypeNode c
  = GQLTypeNode (TypeDefinition c CONST)
  | GQLDirectiveNode (DirectiveDefinition CONST)

nodeToType :: (Applicative f, MonadError GQLError f) => GQLTypeNode c -> f (TypeDefinition c CONST)
nodeToType node = case node of
  GQLTypeNode x -> pure x
  GQLDirectiveNode dir -> throwError $ "expected " <> msg (directiveDefinitionName dir) <> " to be a type but its directive!"
