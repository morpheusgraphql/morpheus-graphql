module Data.Morpheus.PreProcess.Arguments
  ( validateArguments
  , resolveArguments
  , onlyResolveArguments
  ) where

import           Data.List                              ((\\))
import           Data.Morpheus.Error.Arguments          (argumentError, requiredArgument,
                                                         unknownArguments)
import           Data.Morpheus.PreProcess.Input.Enum    (validateEnum)
import           Data.Morpheus.PreProcess.Utils         (existsType)
import           Data.Morpheus.PreProcess.Variable      (replaceVariable)
import qualified Data.Morpheus.Schema.Field             as F (args, name)
import qualified Data.Morpheus.Schema.InputValue        as I (inputValueMeta, isRequired, name)
import qualified Data.Morpheus.Schema.Type              as T (kind)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import qualified Data.Morpheus.Schema.Utils.InputValue  as UI (typeName)
import           Data.Morpheus.Schema.Utils.Utils       (Field, InputValue, TypeLib)
import           Data.Morpheus.Types.Describer          (EnumOf (..))
import           Data.Morpheus.Types.Error              (MetaValidation, Validation)
import           Data.Morpheus.Types.JSType             (JSType (..))
import qualified Data.Morpheus.Types.Query.RawSelection as Raw (RawArguments)
import           Data.Morpheus.Types.Query.Selection    (Argument (..), Arguments)
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Text                              (Text)

asGQLError :: MetaValidation a -> Validation a
asGQLError (Left err)    = Left $ argumentError err
asGQLError (Right value) = pure value

-- TODO: Validate other Types , type missmatch
checkArgumentType :: TypeLib -> Text -> (Text, Argument) -> Validation (Text, Argument)
checkArgumentType typeLib typeName (aKey, argument) = asGQLError (existsType typeName typeLib) >>= checkType
  where
    checkType _type =
      case T.kind _type of
        EnumOf ENUM -> validateEnum _type argument >>= \x -> pure (aKey, x)
       -- INPUT_OBJECT is already validated
        _           -> pure (aKey, argument)

validateArgument :: TypeLib -> Arguments -> InputValue -> Validation (Text, Argument)
validateArgument types requestArgs inpValue =
  case lookup (I.name inpValue) requestArgs of
    Nothing ->
      if I.isRequired inpValue
        then Left $ requiredArgument (I.inputValueMeta 0 inpValue)
        else pure (key, Argument JSNull 0)
    Just x -> checkArgumentType types (UI.typeName inpValue) (key, x)
  where
    key = I.name inpValue

onlyResolveArguments :: GQLQueryRoot -> Raw.RawArguments -> Validation Arguments
onlyResolveArguments root = mapM (replaceVariable root)

checkForUnknownArguments :: Field -> [(Text, a)] -> Validation [InputValue]
checkForUnknownArguments field args =
  case map fst args \\ map I.name (F.args field) of
    []          -> pure $ F.args field
    unknownArgs -> Left $ unknownArguments (F.name field) unknownArgs

resolveArguments :: TypeLib -> GQLQueryRoot -> Field -> Raw.RawArguments -> Validation Arguments
resolveArguments typeLib root inputs args = onlyResolveArguments root args >>= validateArguments typeLib inputs

validateArguments :: TypeLib -> Field -> Arguments -> Validation Arguments
validateArguments typeLib inputs args = checkForUnknownArguments inputs args >>= mapM (validateArgument typeLib args)
