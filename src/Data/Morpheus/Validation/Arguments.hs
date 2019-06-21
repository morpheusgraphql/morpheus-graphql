{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Arguments
  ( validateArguments
  ) where

import           Data.Morpheus.Error.Arguments                 (argumentGotInvalidValue, argumentNameCollision,
                                                                undefinedArgument, unknownArguments)
import           Data.Morpheus.Error.Input                     (InputValidation, inputErrorMessage)
import           Data.Morpheus.Error.Internal                  (internalUnknownTypeMessage)
import           Data.Morpheus.Error.Variable                  (undefinedVariable)
import           Data.Morpheus.Types.Internal.AST.Operator     (ValidVariables, Variable (..))
import           Data.Morpheus.Types.Internal.AST.RawSelection (RawArgument (..), RawArguments, Reference (..))
import           Data.Morpheus.Types.Internal.AST.Selection    (Argument (..), Arguments)
import           Data.Morpheus.Types.Internal.Base             (EnhancedKey (..), Position)
import           Data.Morpheus.Types.Internal.Data             (DataArgument, DataField (..), DataInputField,
                                                                DataOutputField, DataTypeLib, DataTypeWrapper (..),
                                                                isFieldNullable)
import           Data.Morpheus.Types.Internal.Validation       (Validation)
import           Data.Morpheus.Types.Internal.Value            (Value (Null))
import           Data.Morpheus.Validation.Input.Object         (validateInputValue)
import           Data.Morpheus.Validation.Utils.Utils          (checkForUnknownKeys, checkNameCollision, getInputType)
import           Data.Text                                     (Text)

resolveArgumentVariables :: ValidVariables -> DataOutputField -> RawArguments -> Validation Arguments
resolveArgumentVariables variables DataField {fieldArgs} = mapM resolveArgumentValue
  where
    resolveArgumentValue :: (Text, RawArgument) -> Validation (Text, Argument)
    resolveArgumentValue (key', RawArgument argument') = pure (key', argument')
    resolveArgumentValue (key', VariableReference Reference {referenceName, referencePosition}) = do
      value <- lookupVar
      pure (key', Argument value referencePosition)
      where
        lookupVar =
          case lookup referenceName variables of
            Nothing -> Left $ undefinedVariable "Query" referencePosition key' -- TODO
            Just Variable {variableValue, variableType, variableTypeWrappers} ->
              case lookup key' fieldArgs of
                Nothing -> Left $ undefinedVariable "Query" referencePosition key' -- TODO
                Just DataField {fieldType, fieldTypeWrappers}
                  | variableType == fieldType && stricter variableTypeWrappers fieldTypeWrappers -> return variableValue
                  where stricter [] []                               = True
                        stricter (NonNullType:xs1) (NonNullType:xs2) = stricter xs1 xs2
                        stricter (NonNullType:xs1) xs2               = stricter xs1 xs2
                        stricter (ListType:xs1) (ListType:xs2)       = stricter xs1 xs2
                        stricter _ _                                 = False
                _ -> Left $ undefinedVariable "NNN" referencePosition key' -- TODO

handleInputError :: Text -> Position -> InputValidation a -> Validation ()
handleInputError key' position' (Left error') = Left $ argumentGotInvalidValue key' (inputErrorMessage error') position'
handleInputError _ _ _ = pure ()

validateArgumentValue :: DataTypeLib -> DataField a -> (Text, Argument) -> Validation (Text, Argument)
validateArgumentValue lib' DataField {fieldType = typeName', fieldTypeWrappers = wrappers'} (key', Argument value' position') =
  getInputType typeName' lib' (internalUnknownTypeMessage typeName') >>= checkType >>
  pure (key', Argument value' position')
  where
    checkType type' = handleInputError key' position' (validateInputValue lib' [] wrappers' type' (key', value'))

validateArgument :: DataTypeLib -> Position -> Arguments -> (Text, DataArgument) -> Validation (Text, Argument)
validateArgument types position' requestArgs (key', arg) =
  case lookup key' requestArgs of
    Nothing                   -> handleNullable
    Just (Argument Null _)    -> handleNullable
    Just (Argument value pos) -> validateArgumentValue types arg (key', Argument value pos)
  where
    handleNullable =
      if isFieldNullable arg
        then pure (key', Argument Null position')
        else Left $ undefinedArgument (EnhancedKey key' position')

checkForUnknownArguments :: (Text, DataOutputField) -> Arguments -> Validation [(Text, DataInputField)]
checkForUnknownArguments (fieldKey', DataField {fieldArgs = astArgs'}) args' =
  checkForUnknownKeys enhancedKeys' fieldKeys error' >> checkNameCollision enhancedKeys' fieldKeys argumentNameCollision >>
  pure astArgs'
  where
    error' = unknownArguments fieldKey'
    enhancedKeys' = map argToKey args'
    argToKey (key', Argument _ pos) = EnhancedKey key' pos
    fieldKeys = map fst astArgs'

validateArguments ::
     DataTypeLib -> ValidVariables -> (Text, DataOutputField) -> Position -> RawArguments -> Validation Arguments
validateArguments typeLib variables inputs pos rawArgs = do
  args <- resolveArgumentVariables variables (snd inputs) rawArgs
  dataArgs <- checkForUnknownArguments inputs args
  mapM (validateArgument typeLib pos args) dataArgs
