{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Arguments
  ( validateArguments
  , resolveArguments
  , onlyResolveArguments
  ) where

import           Data.Morpheus.Error.Arguments          (argumentGotInvalidValue, requiredArgument, unknownArguments)
import           Data.Morpheus.Error.Input              (InputValidation, inputErrorMessage)
import           Data.Morpheus.Error.Internal           (internalUnknownTypeMessage)
import           Data.Morpheus.PreProcess.Input.Object  (validateInput)
import           Data.Morpheus.PreProcess.Utils         (differKeys, getInputType)
import           Data.Morpheus.PreProcess.Variable      (replaceVariable)
import           Data.Morpheus.Schema.Internal.Types    (Field (..), InputField (..), ObjectField (..), TypeLib)
import           Data.Morpheus.Types.Core               (EnhancedKey (..))
import           Data.Morpheus.Types.Error              (Validation)
import           Data.Morpheus.Types.JSType             (JSType (JSNull))
import           Data.Morpheus.Types.MetaInfo           (MetaInfo (..), Position)
import qualified Data.Morpheus.Types.Query.RawSelection as Raw (RawArguments)
import           Data.Morpheus.Types.Query.Selection    (Argument (..), Arguments)
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Text                              (Text)

handleInputError :: Text -> Int -> InputValidation a -> Validation ()
handleInputError key' position' (Left error') = Left $ argumentGotInvalidValue key' (inputErrorMessage error') position'
handleInputError _ _ _ = pure ()

validateArgumentValue :: TypeLib -> Text -> (Text, Argument) -> Validation (Text, Argument)
validateArgumentValue lib' typeID' (key', Argument value' position') =
  getInputType typeID' lib' (internalUnknownTypeMessage typeID') >>= checkType >> pure (key', Argument value' position')
  where
    checkType type' = handleInputError key' position' (validateInput lib' type' (key', value'))

validateArgument :: TypeLib -> Position -> Arguments -> (Text, InputField) -> Validation (Text, Argument)
validateArgument types position' requestArgs (key', InputField arg) =
  case lookup key' requestArgs of
    Nothing ->
      if notNull arg
        then Left $ requiredArgument (MetaInfo {position = position', key = "TODO:", typeName = "TODO:"})
        else pure (key', Argument JSNull position')
    Just (Argument value pos) -> validateArgumentValue types (fieldType arg) (key', Argument value pos)

onlyResolveArguments :: GQLQueryRoot -> Position -> Raw.RawArguments -> Validation Arguments
onlyResolveArguments root _ = mapM (replaceVariable root)

checkForUnknownArguments :: (Text, ObjectField) -> Arguments -> Validation [(Text, InputField)]
checkForUnknownArguments (fieldKey', ObjectField fieldArgs _) args' =
  case differKeys (map argToKey args') fieldKeys of
    []          -> pure fieldArgs
    unknownArgs -> Left $ unknownArguments fieldKey' unknownArgs
  where
    argToKey (key', Argument _ pos) = EnhancedKey key' pos
    fieldKeys = map fst fieldArgs

resolveArguments ::
     TypeLib -> GQLQueryRoot -> (Text, ObjectField) -> Position -> Raw.RawArguments -> Validation Arguments
resolveArguments typeLib root objectField pos args' =
  onlyResolveArguments root pos args' >>= validateArguments typeLib objectField pos

validateArguments :: TypeLib -> (Text, ObjectField) -> Position -> Arguments -> Validation Arguments
validateArguments typeLib inputs pos args' =
  checkForUnknownArguments inputs args' >>= mapM (validateArgument typeLib pos args')
