{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Validation.Internal.Arguments
  ( validateDirectiveArguments,
    validateFieldArguments,
    ArgumentsConstraints,
    ResolveArgument (..),
    ValidateWithDefault (..),
  )
where

import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Morpheus.Internal.Utils
  ( empty,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    ArgumentDefinition,
    Arguments,
    ArgumentsDefinition (..),
    CONST,
    DirectiveDefinition,
    DirectiveDefinition (..),
    FieldDefinition (..),
    IN,
    OUT,
    ObjectEntry (..),
    Position (..),
    RAW,
    RawValue,
    ResolvedValue,
    Schema,
    VALID,
    Value (..),
    VariableDefinitions,
    fieldContentArgs,
  )
import Data.Morpheus.Types.Internal.Validation
  ( GetWith,
    InputContext,
    InputSource (..),
    MissingRequired,
    Scope (..),
    Validator,
    askVariables,
    asksScope,
    selectKnown,
    selectRequired,
    selectWithDefaultValue,
    startInput,
    withPosition,
  )
import Data.Morpheus.Validation.Internal.Value
  ( Validate,
    ValueContext (..),
    validateInputByField,
  )

type VariableConstraints ctx =
  ( GetWith ctx (VariableDefinitions VALID),
    MissingRequired (VariableDefinitions VALID) ctx
  )

type ArgumentConstraints ctx s =
  ( GetWith ctx (Schema s),
    Validate (ValueContext s) Value CONST (InputContext ctx)
  )

type ArgumentsConstraints ctx s =
  ( VariableConstraints ctx,
    ArgumentConstraints ctx s
  )

-- only Resolves , doesnot checks the types
resolveObject ::
  VariableConstraints ctx =>
  RawValue ->
  Validator ctx ResolvedValue
resolveObject = resolve
  where
    resolveEntry ::
      VariableConstraints ctx =>
      ObjectEntry RAW ->
      Validator ctx (ObjectEntry CONST)
    resolveEntry (ObjectEntry name v) = ObjectEntry name <$> resolve v
    ------------------------------------------------

    resolve ::
      VariableConstraints ctx =>
      RawValue ->
      Validator ctx ResolvedValue
    resolve Null = pure Null
    resolve (Scalar x) = pure $ Scalar x
    resolve (Enum x) = pure $ Enum x
    resolve (List x) = List <$> traverse resolve x
    resolve (Object obj) = Object <$> traverse resolveEntry obj
    resolve (VariableValue ref) =
      askVariables
        >>= fmap (ResolvedVariable ref)
          . selectRequired ref

class ValidateWithDefault schemaStage ctx where
  validateArgument ::
    Arguments CONST ->
    ArgumentDefinition schemaStage ->
    Validator ctx (Argument VALID)

instance
  GetWith ctx (Schema VALID) =>
  ValidateWithDefault VALID ctx
  where
  validateArgument
    requestArgs
    argumentDef@FieldDefinition
      { fieldName
      } =
      selectWithDefaultValue
        f
        (validateArgumentValue argumentDef)
        argumentDef
        requestArgs
      where
        f :: Value VALID -> Validator ctx (Argument VALID)
        f value = do
          argumentPosition <- fromMaybe (Position 0 0) <$> asksScope position
          pure Argument {argumentName = fieldName, argumentValue = value, argumentPosition}

instance
  ArgumentConstraints ctx CONST =>
  ValidateWithDefault CONST ctx
  where
  validateArgument
    requestArgs
    argumentDef@FieldDefinition
      { fieldName
      } =
      selectWithDefaultValue
        f
        (validateArgumentValue argumentDef)
        argumentDef
        requestArgs
      where
        f :: Value CONST -> Validator ctx (Argument VALID)
        f value = do
          argumentPosition <- fromMaybe (Position 0 0) <$> asksScope position
          let arg = Argument {argumentName = fieldName, argumentValue = value, argumentPosition}
          validateArgumentValue argumentDef arg

validateArgumentValue ::
  ArgumentConstraints ctx schemaStage =>
  FieldDefinition IN schemaStage ->
  Argument CONST ->
  Validator ctx (Argument VALID)
validateArgumentValue
  argumentDef
  arg@Argument {argumentValue = value, ..} =
    withPosition argumentPosition
      $ startInput (SourceArgument arg)
      $ do
        argumentValue <- validateInputByField argumentDef value
        pure Argument {argumentValue, ..}

validateFieldArguments ::
  forall ctx.
  ( GetWith ctx (VariableDefinitions VALID),
    MissingRequired (VariableDefinitions VALID) ctx,
    GetWith ctx (Schema VALID)
  ) =>
  FieldDefinition OUT VALID ->
  Arguments RAW ->
  Validator ctx (Arguments VALID)
validateFieldArguments fieldDef@FieldDefinition {fieldContent} =
  validateArguments f argsDef
  where
    f :: Argument CONST -> Validator ctx (ArgumentDefinition VALID)
    f = (`selectKnown` fieldDef)
    argsDef = maybe empty fieldContentArgs fieldContent

validateDirectiveArguments ::
  forall ctx schemaStage valueStage.
  ( ResolveArgument valueStage ctx,
    GetWith ctx (Schema schemaStage),
    ValidateWithDefault schemaStage ctx
  ) =>
  DirectiveDefinition schemaStage ->
  Arguments valueStage ->
  Validator ctx (Arguments VALID)
validateDirectiveArguments
  directiveDef@DirectiveDefinition
    { directiveDefinitionArgs
    } =
    validateArguments f directiveDefinitionArgs
    where
      f :: Argument CONST -> Validator ctx (ArgumentDefinition schemaStage)
      f = (`selectKnown` directiveDef)

validateArguments ::
  ( ResolveArgument s ctx,
    GetWith ctx (Schema schemaStage),
    ValidateWithDefault schemaStage ctx
  ) =>
  (Argument CONST -> Validator ctx (ArgumentDefinition schemaStage)) ->
  ArgumentsDefinition schemaStage ->
  Arguments s ->
  Validator ctx (Arguments VALID)
validateArguments checkUnknown argsDef rawArgs = do
  args <- traverse resolveArgument rawArgs
  traverse_ checkUnknown args
  traverse (validateArgument args) (arguments argsDef)

class ResolveArgument s ctx where
  resolveArgument ::
    Argument s ->
    Validator ctx (Argument CONST)

instance
  VariableConstraints ctx =>
  ResolveArgument RAW ctx
  where
  resolveArgument (Argument key val position) = do
    constValue <- resolveObject val
    pure $ Argument key constValue position

instance ResolveArgument CONST ctx where
  resolveArgument = pure
