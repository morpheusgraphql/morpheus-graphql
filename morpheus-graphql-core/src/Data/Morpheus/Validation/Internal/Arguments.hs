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
    Object,
    ObjectEntry (..),
    Position (..),
    RAW,
    RawValue,
    ResolvedValue,
    Schema,
    TypeRef (..),
    TypeWrapper,
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
    askInputFieldType,
    askVariables,
    asksScope,
    selectKnown,
    selectRequired,
    selectWithDefaultValue,
    startInput,
    withPosition,
  )
import qualified Data.Morpheus.Validation.Internal.Value as V
import Data.Morpheus.Validation.Internal.Value (ValueContext (..))

type VariableConstraints ctx =
  ( GetWith ctx (VariableDefinitions VALID),
    MissingRequired (VariableDefinitions VALID) ctx
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

type ValueConstraints ctx s =
  ( GetWith ctx (Schema s),
    MissingRequired (Object CONST) (InputContext ctx)
  )

type ArgumentConstraints ctx s =
  ( MissingRequired (Arguments CONST) ctx,
    ValueConstraints ctx s,
    V.Validate (V.ValueContext s) ObjectEntry CONST (InputContext ctx)
  )

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
        (__validate argumentDef)
        argumentDef
        requestArgs
      where
        f :: Value VALID -> Validator ctx (Argument VALID)
        f value = do
          argumentPosition <- fromMaybe (Position 0 0) <$> asksScope position
          pure Argument {argumentName = fieldName, argumentValue = value, argumentPosition}

instance
  ArgConst ctx CONST =>
  ValidateWithDefault CONST ctx
  where
  validateArgument
    requestArgs
    argumentDef@FieldDefinition
      { fieldName,
        fieldType = TypeRef {typeWrappers}
      } =
      selectWithDefaultValue
        f
        (__validate argumentDef)
        argumentDef
        requestArgs
      where
        f :: Value CONST -> Validator ctx (Argument VALID)
        f value = do
          argumentPosition <- fromMaybe (Position 0 0) <$> asksScope position
          let arg = Argument {argumentName = fieldName, argumentValue = value, argumentPosition}
          validateArgumentValue argumentDef typeWrappers arg

type ArgConst ctx s =
  ( GetWith ctx (Schema s),
    V.Validate (ValueContext s) Value CONST (InputContext ctx)
  )

__validate ::
  ArgConst ctx s =>
  FieldDefinition IN s ->
  Argument CONST ->
  Validator ctx (Argument VALID)
__validate
  argumentDef@FieldDefinition
    { fieldType = TypeRef {typeWrappers}
    } = validateArgumentValue argumentDef typeWrappers

validateArgumentValue ::
  ArgConst ctx schemaStage =>
  FieldDefinition IN schemaStage ->
  [TypeWrapper] ->
  Argument CONST ->
  Validator ctx (Argument VALID)
validateArgumentValue
  argumentDef
  typeWrappers
  arg@Argument {argumentValue = value, ..} =
    withPosition argumentPosition
      $ startInput (SourceArgument arg)
      $ do
        valueTypeDef <- askInputFieldType argumentDef
        let valueContext = ValueContext {valueWrappers = typeWrappers, valueTypeDef}
        argumentValue <- V.validate valueContext value
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

type ArgumentsConstraints ctx s =
  ( VariableConstraints ctx,
    ArgumentConstraints ctx s
  )

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
