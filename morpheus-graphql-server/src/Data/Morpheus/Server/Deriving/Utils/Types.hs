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
    coerceInputObject,
    coerceScalar,
    getField,
    handleEither,
    coerceArguments,
    coerceObject,
    toFieldContent,
  )
where

import Control.Monad.Except (MonadError (..), throwError)
import Data.Morpheus.Internal.Ext
  ( GQLResult,
    Result (Failure, Success, errors),
  )
import Data.Morpheus.Internal.Utils (selectOr)
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
  )
import Data.Morpheus.Types.GQLScalar
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    Argument (..),
    Arguments,
    ArgumentsDefinition,
    CONST,
    DirectiveDefinition (..),
    FieldContent (..),
    FieldName,
    FieldsDefinition,
    GQLError,
    IN,
    Msg (..),
    OBJECT,
    OUT,
    ObjectEntry (..),
    Position (..),
    ScalarValue,
    Schema (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    VALID,
    ValidObject,
    ValidValue,
    Value (..),
    fieldsToArguments,
    internal,
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

getField :: FieldName -> ValidObject -> ValidValue
getField = selectOr Null entryValue

handleEither :: (MonadError GQLError m, Msg t) => Either t a -> m a
handleEither = either (throwError . msg) pure

typeToArguments :: (DerivingMonad m) => TypeDefinition IN CONST -> m (ArgumentsDefinition CONST)
typeToArguments TypeDefinition {typeContent = DataInputObject {inputObjectFields}} = pure $ fieldsToArguments inputObjectFields
typeToArguments TypeDefinition {typeName} = failureOnlyObject typeName

toFieldContent :: CatType c a -> ArgumentsDefinition CONST -> Maybe (FieldContent TRUE c CONST)
toFieldContent OutputType x | not (null x) = Just (FieldArgs x)
toFieldContent _ _ = Nothing

-- if value is already validated but value has different type
typeMismatch :: GQLError -> Value s -> GQLError
typeMismatch text jsType =
  internal
    $ "Type mismatch! expected:"
    <> text
    <> ", got: "
    <> msg jsType

coerceInputObject :: (MonadError GQLError m) => ValidValue -> m ValidObject
coerceInputObject (Object object) = pure object
coerceInputObject isType = throwError (typeMismatch "InputObject" isType)

coerceScalar :: (MonadError GQLError m) => TypeName -> Value VALID -> m ScalarValue
coerceScalar typename value = case toScalar value of
  Right scalar -> pure scalar
  Left message ->
    throwError
      ( typeMismatch
          ("SCALAR(" <> msg typename <> ")" <> msg message)
          value
      )

coerceArguments :: (MonadError GQLError m) => Value s -> m (Arguments s)
coerceArguments (Object v) = pure $ fmap (\ObjectEntry {..} -> Argument (Position 0 0) entryName entryValue) v
coerceArguments _ = throwError $ internal "could not encode arguments. Arguments should be an object like type!"

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

nodeToType :: (DerivingMonad m) => GQLTypeNode c -> m (TypeDefinition c CONST)
nodeToType node = case node of
  GQLTypeNode x _ -> pure x
  GQLDirectiveNode dir -> throwError $ "expected " <> msg (directiveDefinitionName dir) <> " to be a type but its directive!"

coerceObject :: (DerivingMonad m) => TypeDefinition c CONST -> m (TypeDefinition OBJECT CONST)
coerceObject TypeDefinition {..} = do
  x <- withObject typeName typeContent
  pure (TypeDefinition {typeContent = DataObject [] x, ..})

data NodeTypeVariant
  = NodeTypeVariant TypeName (TypeContent TRUE ANY CONST)
  | NodeUnitType
