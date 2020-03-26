{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Data.Morpheus.Validation.Query.Arguments
  ( validateArguments
  )
where

import           Data.Maybe                     ( maybe )
import           Data.Morpheus.Error.Arguments  ( argumentGotInvalidValue
                                                , argumentNameCollision
                                                , undefinedArgument
                                                , unknownArguments
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
                                                , RawArgument
                                                , RawArguments
                                                , ValidArgument
                                                , ValidArguments
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
                                                , checkForUnknownKeys
                                                , checkNameCollision
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( Listable(..)
                                                , selectBy
                                                , selectOr
                                                , keys
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
  resolve :: RawValue -> Validation ResolvedValue
  resolve Null         = pure Null
  resolve (Scalar x  ) = pure $ Scalar x
  resolve (Enum   x  ) = pure $ Enum x
  resolve (List   x  ) = List <$> traverse resolve x
  resolve (Object obj) = Object <$> traverse resolve obj
  resolve (VariableValue ref) =
    ResolvedVariable ref <$> variableByRef operationName variables ref
    --  >>= checkTypeEquality ref fieldType
  -- RAW | RESOLVED | Valid 

variableByRef :: Name -> ValidVariables -> Ref -> Validation (Variable VALID)
variableByRef operationName variables Ref { refName, refPosition } = maybe
  variableError
  pure
  (lookup refName variables)
 where
  variableError = failure $ undefinedVariable operationName refPosition refName

resolveArgumentVariables
  :: Name
  -> ValidVariables
  -> FieldDefinition
  -> RawArguments
  -> Validation (Arguments RESOLVED)
resolveArgumentVariables operationName variables FieldDefinition { fieldName, fieldArgs }
  = mapM resolveVariable
 where
  resolveVariable :: RawArgument -> Validation (Argument RESOLVED)
  resolveVariable (Argument key val position) = do 
    _ <- checkUnknown
    constValue <- resolveObject operationName variables val
    pure $ Argument key constValue position
    where 
      checkUnknown :: Validation FieldDefinition
      checkUnknown = selectBy (unknownArguments fieldName [Ref key position]) key fieldArgs

validateArgument
  :: Schema
  -> Position
  -> Arguments RESOLVED
  -> ArgumentDefinition
  -> Validation ValidArgument
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
  validateArgumentValue :: Argument RESOLVED -> Validation ValidArgument
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
  -> RawArguments
  -> Validation ValidArguments
validateArguments 
    typeLib 
    operatorName 
    variables 
    field@FieldDefinition { fieldArgs = ArgumentsDefinition _ fArgs } 
    pos 
    rawArgs
  = do
    args     <- resolveArgumentVariables operatorName variables field rawArgs
    checkForUnknownArguments args
    traverse (validateArgument typeLib pos args) fArgs
 where
  checkForUnknownArguments
    :: Arguments RESOLVED -> Validation ()
  checkForUnknownArguments args = checkForUnknownKeys enhancedKeys (keys fArgs) argError >> pure ()
   where
    argError     = unknownArguments (fieldName field)
    enhancedKeys = toList $ fmap argToKey args
    argToKey :: Argument RESOLVED -> Ref
    argToKey Argument { argumentName, argumentPosition } = Ref argumentName argumentPosition