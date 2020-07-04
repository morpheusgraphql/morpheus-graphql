{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Validation.Query.Arguments
  ( validateDirectiveArguments,
    validateFieldArguments,
  )
where

import Data.Foldable (traverse_)
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
    FieldsDefinition,
    IN,
    OUT,
    Object,
    ObjectEntry (..),
    RAW,
    RawValue,
    ResolvedValue,
    Schema,
    TypeRef (..),
    VALID,
    Value (..),
    VariableDefinitions,
    fieldContentArgs,
  )
import Data.Morpheus.Types.Internal.Validation
  ( DirectiveValidator,
    GetWith,
    InputContext,
    InputSource (..),
    MissingRequired,
    Scope (..),
    SelectionValidator,
    SetWith,
    Unknown,
    askInputFieldType,
    askVariables,
    asks,
    selectKnown,
    selectRequired,
    selectWithDefaultValue,
    startInput,
    withPosition,
  )
import Data.Morpheus.Validation.Internal.Value
  ( validateInput,
  )

type VariableConstraints ctx =
  ( GetWith ctx (VariableDefinitions VALID),
    MissingRequired (VariableDefinitions VALID) ctx
  )

-- only Resolves , doesnot checks the types
resolveObject ::
  VariableConstraints ctx =>
  RawValue ->
  DirectiveValidator ctx ResolvedValue
resolveObject = resolve
  where
    resolveEntry ::
      VariableConstraints ctx =>
      ObjectEntry RAW ->
      DirectiveValidator ctx (ObjectEntry CONST)
    resolveEntry (ObjectEntry name v) = ObjectEntry name <$> resolve v
    ------------------------------------------------

    resolve ::
      VariableConstraints ctx =>
      RawValue ->
      DirectiveValidator ctx ResolvedValue
    resolve Null = pure Null
    resolve (Scalar x) = pure $ Scalar x
    resolve (Enum x) = pure $ Enum x
    resolve (List x) = List <$> traverse resolve x
    resolve (Object obj) = Object <$> traverse resolveEntry obj
    resolve (VariableValue ref) =
      askVariables
        >>= fmap (ResolvedVariable ref)
          . selectRequired ref

resolveArgumentVariables ::
  VariableConstraints ctx =>
  Arguments RAW ->
  DirectiveValidator ctx (Arguments CONST)
resolveArgumentVariables =
  traverse resolveVariable
  where
    resolveVariable ::
      VariableConstraints ctx =>
      Argument RAW ->
      DirectiveValidator ctx (Argument CONST)
    resolveVariable (Argument key val position) = do
      constValue <- resolveObject val
      pure $ Argument key constValue position

type ValueConstraints ctx =
  ( GetWith ctx Schema,
    GetWith ctx Scope,
    SetWith ctx Scope,
    MissingRequired (Object CONST) (InputContext ctx),
    Unknown (FieldsDefinition IN) (InputContext ctx)
  )

type ArgumentConstraints ctx =
  ( MissingRequired (Arguments CONST) ctx,
    ValueConstraints ctx
  )

validateArgument ::
  ArgumentConstraints ctx =>
  Arguments CONST ->
  ArgumentDefinition ->
  DirectiveValidator ctx (Argument VALID)
validateArgument
  requestArgs
  argumentDef@FieldDefinition
    { fieldName,
      fieldType = TypeRef {typeWrappers}
    } =
    do
      argumentPosition <- asks position
      argument <-
        selectWithDefaultValue
          (\argumentValue -> Argument {argumentName = fieldName, argumentValue, argumentPosition})
          argumentDef
          requestArgs
      validateArgumentValue argument
    where
      -------------------------------------------------------------------------
      validateArgumentValue ::
        ( GetWith ctx Schema,
          GetWith ctx Scope,
          SetWith ctx Scope,
          MissingRequired (Object CONST) (InputContext ctx),
          Unknown (FieldsDefinition IN) (InputContext ctx)
        ) =>
        Argument CONST ->
        DirectiveValidator ctx (Argument VALID)
      validateArgumentValue arg@Argument {argumentValue = value, ..} =
        withPosition argumentPosition
          $ startInput (SourceArgument arg)
          $ do
            datatype <- askInputFieldType argumentDef
            argumentValue <-
              validateInput
                typeWrappers
                datatype
                (ObjectEntry fieldName value)
            pure Argument {argumentValue, ..}

validateFieldArguments ::
  FieldDefinition OUT ->
  Arguments RAW ->
  SelectionValidator (Arguments VALID)
validateFieldArguments fieldDef@FieldDefinition {fieldContent} =
  validateArgumengts (`selectKnown` fieldDef) argsDef
  where
    argsDef = maybe empty fieldContentArgs fieldContent

-------------------------------------------------

validateDirectiveArguments ::
  ArgumentsConstraints ctx =>
  DirectiveDefinition ->
  Arguments RAW ->
  DirectiveValidator ctx (Arguments VALID)
validateDirectiveArguments
  directiveDef@DirectiveDefinition
    { directiveDefinitionArgs
    } =
    validateArgumengts
      (`selectKnown` directiveDef)
      directiveDefinitionArgs

type ArgumentsConstraints ctx =
  ( VariableConstraints ctx,
    ArgumentConstraints ctx
  )

validateArgumengts ::
  ArgumentsConstraints ctx =>
  (Argument CONST -> DirectiveValidator ctx ArgumentDefinition) ->
  ArgumentsDefinition ->
  Arguments RAW ->
  DirectiveValidator ctx (Arguments VALID)
validateArgumengts checkUnknown argsDef rawArgs =
  do
    args <- resolveArgumentVariables rawArgs
    traverse_ checkUnknown args
    traverse (validateArgument args) (arguments argsDef)
