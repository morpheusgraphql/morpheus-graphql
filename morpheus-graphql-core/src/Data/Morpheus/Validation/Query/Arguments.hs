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
  ( elems,
  )
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    ArgumentDefinition,
    Arguments,
    ArgumentsDefinition (..),
    DirectiveDefinition,
    DirectiveDefinition (..),
    FieldDefinition (..),
    OUT,
    ObjectEntry (..),
    RAW,
    RESOLVED,
    RawValue,
    ResolvedValue,
    TypeRef (..),
    VALID,
    Value (..),
    fieldContentArgs,
  )
import Data.Morpheus.Types.Internal.Validation
  ( InputSource (..),
    SelectionContext (..),
    SelectionValidator,
    askContext,
    askInputFieldType,
    askScopePosition,
    selectKnown,
    selectRequired,
    selectWithDefaultValue,
    startInput,
    withScopePosition,
  )
import Data.Morpheus.Validation.Internal.Value
  ( validateInput,
  )

-- only Resolves , doesnot checks the types
resolveObject :: RawValue -> SelectionValidator ResolvedValue
resolveObject = resolve
  where
    resolveEntry :: ObjectEntry RAW -> SelectionValidator (ObjectEntry RESOLVED)
    resolveEntry (ObjectEntry name v) = ObjectEntry name <$> resolve v
    ------------------------------------------------
    resolve :: RawValue -> SelectionValidator ResolvedValue
    resolve Null = pure Null
    resolve (Scalar x) = pure $ Scalar x
    resolve (Enum x) = pure $ Enum x
    resolve (List x) = List <$> traverse resolve x
    resolve (Object obj) = Object <$> traverse resolveEntry obj
    resolve (VariableValue ref) =
      variables <$> askContext
        >>= fmap (ResolvedVariable ref)
          . selectRequired ref

resolveArgumentVariables ::
  Arguments RAW ->
  SelectionValidator (Arguments RESOLVED)
resolveArgumentVariables =
  traverse resolveVariable
  where
    resolveVariable :: Argument RAW -> SelectionValidator (Argument RESOLVED)
    resolveVariable (Argument key val position) = do
      constValue <- resolveObject val
      pure $ Argument key constValue position

validateArgument ::
  Arguments RESOLVED ->
  ArgumentDefinition ->
  SelectionValidator (Argument VALID)
validateArgument
  requestArgs
  argumentDef@FieldDefinition
    { fieldName,
      fieldType = TypeRef {typeWrappers}
    } =
    do
      argumentPosition <- askScopePosition
      argument <-
        selectWithDefaultValue
          Argument {argumentName = fieldName, argumentValue = Null, argumentPosition}
          argumentDef
          requestArgs
      validateArgumentValue argument
    where
      -------------------------------------------------------------------------
      validateArgumentValue :: Argument RESOLVED -> SelectionValidator (Argument VALID)
      validateArgumentValue arg@Argument {argumentValue = value, ..} =
        withScopePosition argumentPosition
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
validateFieldArguments
  fieldDef@FieldDefinition {fieldContent}
  rawArgs =
    do
      args <- resolveArgumentVariables rawArgs
      traverse_ checkUnknown (elems args)
      traverse (validateArgument args) argsDef
    where
      argsDef = fieldContentArgs fieldContent
      -------------------------------------------------
      checkUnknown :: Argument RESOLVED -> SelectionValidator ArgumentDefinition
      checkUnknown = (`selectKnown` fieldDef)

validateDirectiveArguments ::
  DirectiveDefinition ->
  Arguments RAW ->
  SelectionValidator (Arguments VALID)
validateDirectiveArguments
  directiveDef@DirectiveDefinition
    { directiveDefinitionArgs = ArgumentsDefinition _ argsDef
    }
  rawArgs =
    do
      args <- resolveArgumentVariables rawArgs
      traverse_ checkUnknown (elems args)
      traverse (validateArgument args) argsDef
    where
      -------------------------------------------------
      checkUnknown :: Argument RESOLVED -> SelectionValidator ArgumentDefinition
      checkUnknown = (`selectKnown` directiveDef)
