module Data.Morpheus.Validation.Validate.Arguments
  ( validateArguments
  ) where

import           Data.Morpheus.Error.Arguments         (argumentGotInvalidValue, argumentNameCollision,
                                                        undefinedArgument, unknownArguments)
import           Data.Morpheus.Error.Input             (InputValidation, inputErrorMessage)
import           Data.Morpheus.Error.Internal          (internalUnknownTypeMessage)
import           Data.Morpheus.Schema.Internal.AST     (Field (..), InputField (..), ObjectField (..), TypeLib)
import           Data.Morpheus.Types.Core              (EnhancedKey (..))
import           Data.Morpheus.Types.Error             (Validation)
import           Data.Morpheus.Types.JSType            (JSType (JSNull))
import           Data.Morpheus.Types.MetaInfo          (Position)
import           Data.Morpheus.Types.Query.Selection   (Argument (..), Arguments)
import           Data.Morpheus.Validation.Input.Object (validateInputValue)
import           Data.Morpheus.Validation.Utils        (checkForUnknownKeys, checkNameCollision, getInputType)
import           Data.Text                             (Text)

handleInputError :: Text -> Int -> InputValidation a -> Validation ()
handleInputError key' position' (Left error') = Left $ argumentGotInvalidValue key' (inputErrorMessage error') position'
handleInputError _ _ _ = pure ()

validateArgumentValue :: Bool -> TypeLib -> Text -> (Text, Argument) -> Validation (Text, Argument)
validateArgumentValue isList' lib' typeID' (key', Argument value' position') =
  getInputType typeID' lib' (internalUnknownTypeMessage typeID') >>= checkType >> pure (key', Argument value' position')
  where
    checkType type' = handleInputError key' position' (validateInputValue isList' lib' type' (key', value'))

validateArgument :: TypeLib -> Position -> Arguments -> (Text, InputField) -> Validation (Text, Argument)
validateArgument types position' requestArgs (key', InputField arg) =
  case lookup key' requestArgs of
    Nothing ->
      if notNull arg
        then Left $ undefinedArgument (EnhancedKey key' position')
        else pure (key', Argument JSNull position')
    Just (Argument value pos) -> validateArgumentValue (asList arg) types (fieldType arg) (key', Argument value pos)

checkForUnknownArguments :: (Text, ObjectField) -> Arguments -> Validation [(Text, InputField)]
checkForUnknownArguments (fieldKey', ObjectField fieldArgs _) args' =
  checkForUnknownKeys enhancedKeys' fieldKeys error' >> checkNameCollision enhancedKeys' fieldKeys argumentNameCollision >>
  pure fieldArgs
  where
    error' = unknownArguments fieldKey'
    enhancedKeys' = map argToKey args'
    argToKey (key', Argument _ pos) = EnhancedKey key' pos
    fieldKeys = map fst fieldArgs

validateArguments :: TypeLib -> (Text, ObjectField) -> Position -> Arguments -> Validation Arguments
validateArguments typeLib inputs pos args' =
  checkForUnknownArguments inputs args' >>= mapM (validateArgument typeLib pos args')
