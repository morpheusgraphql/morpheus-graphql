{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Validation.Internal.Arguments
  ( validateDirectiveArguments,
    validateFieldArguments,
    ArgumentsConstraints,
    Resolve,
  )
where

import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    ArgumentDefinition (..),
    Arguments,
    ArgumentsDefinition,
    CONST,
    DirectiveDefinition (..),
    FieldDefinition (..),
    IN,
    OUT,
    ObjectEntry (..),
    Position (..),
    RAW,
    VALID,
    Value (..),
    VariableDefinitions,
    fieldArguments,
    typed,
  )
import Data.Morpheus.Types.Internal.Validation
  ( FragmentValidator,
    InputSource (..),
    MissingRequired,
    OperationContext,
    Scope (..),
    Validator,
    askVariables,
    asksScope,
    selectKnown,
    selectRequired,
    selectWithDefaultValue,
    setPosition,
    startInput,
    withScope,
  )
import Data.Morpheus.Validation.Internal.Value
  ( ValidateWithDefault,
    validateInputByTypeRef,
  )
import Relude hiding (empty)

type VariableConstraints ctx =
  (MissingRequired (VariableDefinitions VALID) ctx)

type ArgumentsConstraints c schemaS valueS =
  ( Resolve Argument valueS c,
    ValidateWithDefault c schemaS schemaS,
    ValidateWithDefault c schemaS CONST
  )

validateArgument ::
  ( ValidateWithDefault ctx schemaS valueS,
    ValidateWithDefault ctx schemaS schemaS
  ) =>
  Arguments valueS ->
  ArgumentDefinition schemaS ->
  Validator schemaS ctx (Argument VALID)
validateArgument
  requestArgs
  ArgumentDefinition {argument} =
    selectWithDefaultValue
      (toArgument argument >=> validateArgumentValue argument)
      (validateArgumentValue argument)
      argument
      requestArgs

toArgument :: FieldDefinition IN s -> Value schemaS -> Validator schemaStage ctx (Argument schemaS)
toArgument
  FieldDefinition {fieldName}
  value = mkArg . fromMaybe (Position 0 0) <$> asksScope position
    where
      mkArg pos = Argument pos fieldName value

validateArgumentValue ::
  (ValidateWithDefault ctx schemaS valueS) =>
  FieldDefinition IN schemaS ->
  Argument valueS ->
  Validator schemaS ctx (Argument VALID)
validateArgumentValue
  field
  Argument {argumentValue, ..} =
    withScope (setPosition argumentPosition)
      $ startInput (SourceArgument argumentName)
      $ Argument
        argumentPosition
        argumentName
      <$> validateInputByTypeRef (typed fieldType field) argumentValue

validateFieldArguments ::
  FieldDefinition OUT VALID ->
  Arguments RAW ->
  FragmentValidator s (Arguments VALID)
validateFieldArguments field =
  validateArguments
    (`selectKnown` arguments)
    arguments
  where
    arguments = fieldArguments field

validateDirectiveArguments ::
  (ArgumentsConstraints ctx schemaStage valueStage) =>
  DirectiveDefinition schemaStage ->
  Arguments valueStage ->
  Validator schemaStage ctx (Arguments VALID)
validateDirectiveArguments
  DirectiveDefinition
    { directiveDefinitionArgs
    } =
    validateArguments
      (`selectKnown` directiveDefinitionArgs)
      directiveDefinitionArgs

validateArguments ::
  (ArgumentsConstraints ctx schemaStage s) =>
  (Argument CONST -> Validator schemaStage ctx (ArgumentDefinition schemaStage)) ->
  ArgumentsDefinition schemaStage ->
  Arguments s ->
  Validator schemaStage ctx (Arguments VALID)
validateArguments checkUnknown argsDef rawArgs = do
  args <- traverse resolve rawArgs
  traverse_ checkUnknown args
    *> traverse (validateArgument args) argsDef

class Resolve f s ctx where
  resolve :: f s -> Validator schemaS ctx (f CONST)

instance (VariableConstraints (OperationContext VALID s)) => Resolve Argument RAW (OperationContext VALID s) where
  resolve (Argument key position val) = Argument key position <$> resolve val

instance Resolve f CONST ctx where
  resolve = pure

instance (VariableConstraints (OperationContext VALID s)) => Resolve Value RAW (OperationContext VALID s) where
  resolve Null = pure Null
  resolve (Scalar x) = pure $ Scalar x
  resolve (Enum x) = pure $ Enum x
  resolve (List elems) = List <$> traverse resolve elems
  resolve (Object fields) = Object <$> traverse resolve fields
  resolve (VariableValue ref) =
    askVariables
      >>= fmap (ResolvedVariable ref)
      . selectRequired ref

instance (VariableConstraints (OperationContext VALID s)) => Resolve ObjectEntry RAW (OperationContext VALID s) where
  resolve (ObjectEntry name value) = ObjectEntry name <$> resolve value
