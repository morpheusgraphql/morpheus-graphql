{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Data.Morpheus.Validation.Query.Arguments
  ( validateArguments
  ) where

import           Data.Morpheus.Error.Arguments                 (argumentGotInvalidValue, argumentNameCollision,
                                                                undefinedArgument, unknownArguments)
import           Data.Morpheus.Error.Input                     (InputValidation, inputErrorMessage)
import           Data.Morpheus.Error.Internal                  (internalUnknownTypeMessage)
import           Data.Morpheus.Error.Variable                  (incompatibleVariableType, undefinedVariable)
import           Data.Morpheus.Types.Internal.AST.Operation    (ValidVariables, Variable (..))
import           Data.Morpheus.Types.Internal.AST.RawSelection (RawArgument (..), RawArguments, Reference (..))
import           Data.Morpheus.Types.Internal.AST.Selection    (Argument (..), ArgumentOrigin (..), Arguments)
import           Data.Morpheus.Types.Internal.Base             (EnhancedKey (..), Position)
import           Data.Morpheus.Types.Internal.Data             (DataArgument, DataField (..), DataInputField,
                                                                DataOutputField, DataTypeLib, isFieldNullable,
                                                                showWrappedType)
import           Data.Morpheus.Types.Internal.Validation       (Validation)
import           Data.Morpheus.Types.Internal.Value            (Value (Null))
import           Data.Morpheus.Validation.Internal.Utils       (checkForUnknownKeys, checkNameCollision, getInputType,
                                                                isEqOrStricter)
import           Data.Morpheus.Validation.Query.Input.Object   (validateInputValue)
import           Data.Text                                     (Text)

resolveArgumentVariables :: Text -> ValidVariables -> DataOutputField -> RawArguments -> Validation Arguments
resolveArgumentVariables operatorName variables DataField {fieldName, fieldArgs} = mapM resolveVariable
  where
    resolveVariable :: (Text, RawArgument) -> Validation (Text, Argument)
    resolveVariable (key, RawArgument argument) = pure (key, argument)
    resolveVariable (key, VariableReference Reference {referenceName, referencePosition}) =
      (key, ) . toArgument <$> lookupVar
      where
        toArgument argumentValue =
          Argument {argumentValue, argumentOrigin = VARIABLE, argumentPosition = referencePosition}
        lookupVar =
          case lookup referenceName variables of
            Nothing -> Left $ undefinedVariable operatorName referencePosition referenceName
            Just Variable {variableValue, variableType, variableTypeWrappers} ->
              case lookup key fieldArgs of
                Nothing -> Left $ unknownArguments fieldName [EnhancedKey key referencePosition]
                Just DataField {fieldType, fieldTypeWrappers} ->
                  if variableType == fieldType && isEqOrStricter variableTypeWrappers fieldTypeWrappers
                    then return variableValue
                    else Left $ incompatibleVariableType referenceName varSignature fieldSignature referencePosition
                  where varSignature = showWrappedType variableTypeWrappers variableType
                        fieldSignature = showWrappedType fieldTypeWrappers fieldType

handleInputError :: Text -> Position -> InputValidation a -> Validation ()
handleInputError key position' (Left error') = Left $ argumentGotInvalidValue key (inputErrorMessage error') position'
handleInputError _ _ _                       = pure ()

validateArgumentValue :: DataTypeLib -> DataField a -> (Text, Argument) -> Validation (Text, Argument)
validateArgumentValue lib DataField {fieldType, fieldTypeWrappers} arg@(key, Argument {argumentValue, argumentPosition}) =
  getInputType fieldType lib (internalUnknownTypeMessage fieldType) >>= checkType >> pure arg
  where
    checkType type' =
      handleInputError key argumentPosition (validateInputValue lib [] fieldTypeWrappers type' (key, argumentValue))

validateArgument :: DataTypeLib -> Position -> Arguments -> (Text, DataArgument) -> Validation (Text, Argument)
validateArgument types argumentPosition requestArgs (key, arg) =
  case lookup key requestArgs of
    Nothing                                            -> handleNullable
    Just argument@Argument {argumentOrigin = VARIABLE} -> pure (key, argument) -- Variables are already checked in Variable Validation
    Just Argument {argumentValue = Null}               -> handleNullable
    Just argument                                      -> validateArgumentValue types arg (key, argument)
  where
    handleNullable
      | isFieldNullable arg = pure (key, Argument {argumentValue = Null, argumentOrigin = INLINE, argumentPosition})
      | otherwise = Left $ undefinedArgument (EnhancedKey key argumentPosition)

checkForUnknownArguments :: (Text, DataOutputField) -> Arguments -> Validation [(Text, DataInputField)]
checkForUnknownArguments (key, DataField {fieldArgs}) args =
  checkForUnknownKeys enhancedKeys fieldKeys argError >> checkNameCollision enhancedKeys argumentNameCollision >>
  pure fieldArgs
  where
    argError = unknownArguments key
    enhancedKeys = map argToKey args
    argToKey (key', Argument {argumentPosition}) = EnhancedKey key' argumentPosition
    fieldKeys = map fst fieldArgs

validateArguments ::
     DataTypeLib
  -> Text
  -> ValidVariables
  -> (Text, DataOutputField)
  -> Position
  -> RawArguments
  -> Validation Arguments
validateArguments typeLib operatorName variables inputs pos rawArgs = do
  args <- resolveArgumentVariables operatorName variables (snd inputs) rawArgs
  dataArgs <- checkForUnknownArguments inputs args
  mapM (validateArgument typeLib pos args) dataArgs
