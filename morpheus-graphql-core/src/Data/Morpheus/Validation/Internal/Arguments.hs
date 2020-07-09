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
    ResolveArgument,
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
    Schema,
    VALID,
    Value (..),
    VariableDefinitions,
    fieldContentArgs,
  )
import Data.Morpheus.Types.Internal.Validation
  ( GetWith,
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
  ( ValidateWithDefault,
    ValueConstraints,
    validateInputByField,
  )

type VariableConstraints ctx =
  ( GetWith ctx (VariableDefinitions VALID),
    MissingRequired (VariableDefinitions VALID) ctx
  )

type ArgumentsConstraints c schemaS valueS =
  ( ResolveArgument valueS c,
    GetWith c (Schema schemaS),
    ValidateWithDefault c schemaS schemaS,
    ValidateWithDefault c schemaS CONST
  )

-- only Resolves , doesnot checks the types
resolveObject ::
  VariableConstraints ctx =>
  Value RAW ->
  Validator ctx (Value CONST)
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
      Value RAW ->
      Validator ctx (Value CONST)
    resolve Null = pure Null
    resolve (Scalar x) = pure $ Scalar x
    resolve (Enum x) = pure $ Enum x
    resolve (List x) = List <$> traverse resolve x
    resolve (Object obj) = Object <$> traverse resolveEntry obj
    resolve (VariableValue ref) =
      askVariables
        >>= fmap (ResolvedVariable ref)
          . selectRequired ref

validateArgument ::
  forall ctx schemaS valueS.
  ( ValueConstraints ctx schemaS valueS,
    ValidateWithDefault ctx schemaS schemaS
  ) =>
  Arguments valueS ->
  FieldDefinition IN schemaS ->
  Validator ctx (Argument VALID)
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
      f :: Value schemaS -> Validator ctx (Argument VALID)
      f value = do
        argumentPosition <- fromMaybe (Position 0 0) <$> asksScope position
        let arg = Argument {argumentName = fieldName, argumentValue = value, argumentPosition}
        validateArgumentValue argumentDef arg

validateArgumentValue ::
  (ValueConstraints ctx schemaS valueS) =>
  FieldDefinition IN schemaS ->
  Argument valueS ->
  Validator ctx (Argument VALID)
validateArgumentValue
  argumentDef
  Argument {argumentValue = value, ..} =
    withPosition argumentPosition
      $ startInput (SourceArgument argumentName)
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
  ArgumentsConstraints ctx schemaStage valueStage =>
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
  ArgumentsConstraints ctx schemaStage s =>
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
