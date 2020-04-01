{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Morpheus.Validation.Query.Arguments
  ( validateArguments
  )
where

import           Data.Foldable                  (traverse_)
import           Data.Morpheus.Error.Arguments  ( argumentGotInvalidValue
                                                -- , argumentNameCollision
                                                , undefinedArgument
                                                )
import           Data.Morpheus.Error.Input      ( InputValidation
                                                , inputErrorMessage
                                                )
import           Data.Morpheus.Error.Internal   ( internalUnknownTypeMessage )
import           Data.Morpheus.Error.Variable   ( undefinedVariable )
import           Data.Morpheus.Types.Internal.AST
                                                ( ValidVariables
                                                , Variable(..)
                                                , Argument(..)
                                                , ArgumentsDefinition(..)
                                                , Arguments
                                                , Ref(..)
                                                , Position
                                                , ArgumentDefinition
                                                , FieldDefinition(..)
                                                , Schema
                                                , TypeRef(..)
                                                , Value(..)
                                                , Name
                                                , RawValue
                                                , ResolvedValue
                                                , RESOLVED
                                                , VALID
                                                , isFieldNullable
                                                , lookupInputType
                                                , ObjectEntry(..)
                                                , RAW
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( Listable(..)
                                                , selectBy
                                                , selectOr
                                                , empty
                                                , selectKnown
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )
import           Data.Morpheus.Validation.Internal.Value
                                                ( validateInputValue )
import           Data.Text                      ( Text )

-- only Resolves , doesnot checks the types
resolveObject :: Name -> ValidVariables -> RawValue -> Validation ResolvedValue
resolveObject operationName variables = resolve
 where
  resolveEntry :: ObjectEntry RAW -> Validation (ObjectEntry RESOLVED)
  resolveEntry (ObjectEntry name v) = ObjectEntry name <$> resolve v
  ------------------------------------------------
  resolve :: RawValue -> Validation ResolvedValue
  resolve Null         = pure Null
  resolve (Scalar x  ) = pure $ Scalar x
  resolve (Enum   x  ) = pure $ Enum x
  resolve (List   x  ) = List <$> traverse resolve x
  resolve (Object obj) = Object <$> traverse resolveEntry obj
  resolve (VariableValue ref) =
    ResolvedVariable ref <$> variableByRef operationName variables ref

variableByRef :: Name -> ValidVariables -> Ref -> Validation (Variable VALID)
variableByRef operationName variables Ref { refName, refPosition } 
  = selectBy variableError refName variables
  where
    variableError = undefinedVariable operationName refPosition refName

resolveArgumentVariables
  :: Name
  -> ValidVariables
  -> Arguments RAW
  -> Validation (Arguments RESOLVED)
resolveArgumentVariables operationName variables
  = traverse resolveVariable
 where
  resolveVariable :: Argument RAW -> Validation (Argument RESOLVED)
  resolveVariable (Argument key val position) = do 
    constValue <- resolveObject operationName variables val
    pure $ Argument key constValue position

validateArgument
  :: Schema
  -> Position
  -> Arguments RESOLVED
  -> ArgumentDefinition
  -> Validation (Argument VALID)
validateArgument lib fieldPosition requestArgs argType@FieldDefinition { fieldName, fieldType = TypeRef { typeConName, typeWrappers } }
  = selectOr 
    handleNullable 
    handleArgument 
    fieldName 
    requestArgs 
 where
    -- TODO: move it in value validation
   -- Just argument@Argument { argumentOrigin = VARIABLE } ->
   --   pure (key, argument) -- Variables are already checked in Variable Validation
  handleArgument Argument { argumentValue = Null } = handleNullable
  handleArgument argument = validateArgumentValue argument
  handleNullable
    | isFieldNullable argType
    = pure Argument { argumentName = fieldName, argumentValue = Null, argumentPosition = fieldPosition }
    | otherwise
    = failure $ undefinedArgument (Ref fieldName fieldPosition)
  -------------------------------------------------------------------------
  validateArgumentValue :: Argument RESOLVED -> Validation (Argument VALID)
  validateArgumentValue Argument { argumentValue = value, .. } =
    do
      datatype <- lookupInputType typeConName
                                  lib
                                  (internalUnknownTypeMessage typeConName)
      argumentValue <- handleInputError
        $ validateInputValue lib [] typeWrappers datatype (fieldName, value)
      pure Argument { argumentValue , .. }
   where
    ---------
    handleInputError :: InputValidation a -> Validation a
    handleInputError (Left err) = failure $ case inputErrorMessage err of
      Left  errors  -> errors
      Right message -> argumentGotInvalidValue fieldName message argumentPosition
    handleInputError (Right x) = pure x

validateArguments
  :: Schema
  -> Text
  -> ValidVariables
  -> FieldDefinition
  -> Position
  -> Arguments RAW
  -> Validation (Arguments VALID)
validateArguments 
    typeLib 
    operatorName 
    variables 
    FieldDefinition { fieldName, fieldArgs }
    pos 
    rawArgs
  = do
    args <- resolveArgumentVariables operatorName variables rawArgs
    traverse_ checkUnknown (toList args)
    traverse (validateArgument typeLib pos args) fArgs
 where
  fArgs = case fieldArgs of 
    (ArgumentsDefinition _ argsD) -> argsD
    NoArguments -> empty
  -------------------------------------------------
  checkUnknown
    :: Argument RESOLVED -> Validation ()
  checkUnknown Argument { argumentName, argumentPosition } 
    = selectKnown (Ref argumentName argumentPosition) fieldArgs >> pure ()
