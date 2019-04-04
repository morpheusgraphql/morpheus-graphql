{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Arguments
  ( validateArguments
  , resolveArguments
  , onlyResolveArguments
  ) where

import           Data.Morpheus.Error.Arguments          (argumentError, requiredArgument, unknownArguments)
import           Data.Morpheus.PreProcess.Input.Object  (validateInput)
import           Data.Morpheus.PreProcess.Utils         (differKeys, getInputType)
import           Data.Morpheus.PreProcess.Variable      (replaceVariable)
import           Data.Morpheus.Schema.Internal.Types    (Field (..), InputField (..), ObjectField (..), TypeLib)
import           Data.Morpheus.Types.Core               (EnhancedKey (..))
import           Data.Morpheus.Types.Error              (MetaValidation, Validation)
import           Data.Morpheus.Types.JSType             (JSType (JSNull))
import           Data.Morpheus.Types.MetaInfo           (MetaInfo (..), Position)
import qualified Data.Morpheus.Types.Query.RawSelection as Raw (RawArguments)
import           Data.Morpheus.Types.Query.Selection    (Argument (..), Arguments)
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Text                              (Text)

asGQLError :: MetaValidation a -> Validation a
asGQLError (Left err)    = Left $ argumentError err
asGQLError (Right value) = pure value

-- TODO: Validate other Types , type Mismatch
checkArgumentType :: TypeLib -> (Text, Int) -> (Text, Argument) -> Validation (Text, Argument)
checkArgumentType lib' (tName, position') (key', Argument value' argPosition) =
  asGQLError (getInputType (position', key') tName lib') >>= checkType >> pure (key', Argument value' argPosition)
    -- error' = expectedTypeAFoundB (MetaInfo {typeName = tName, key = key', position = argPosition})
   -- checkType (Enum tags _) = validateEnum (error' value') tags value'
  where
    checkType type' = asGQLError (validateInput lib' type' argPosition (key', value'))

validateArgument :: TypeLib -> Position -> Arguments -> (Text, InputField) -> Validation (Text, Argument)
validateArgument types position' requestArgs (key', InputField arg) =
  case lookup key' requestArgs of
    Nothing ->
      if notNull arg
        then Left $ requiredArgument (MetaInfo {position = position', key = "TODO:", typeName = "TODO:"})
        else pure (key', Argument JSNull position')
    Just (Argument value pos) -> checkArgumentType types (fieldType arg, pos) (key', Argument value pos)

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
