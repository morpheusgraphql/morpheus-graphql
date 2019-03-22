module Data.Morpheus.PreProcess.Arguments
  ( validateArguments
  , resolveArguments
  , onlyResolveArguments
  ) where

import           Data.Morpheus.Error.Arguments          (argumentError, requiredArgument,
                                                         unknownArguments)
import           Data.Morpheus.PreProcess.Input.Enum    (validateEnum)
import           Data.Morpheus.PreProcess.Utils         (differKeys, existsType)
import           Data.Morpheus.PreProcess.Variable      (replaceVariable)
import qualified Data.Morpheus.Schema.Field             as F (args, name)
import qualified Data.Morpheus.Schema.InputValue        as I (inputValueMeta, isRequired, name)
import qualified Data.Morpheus.Schema.Type              as T (kind)
import           Data.Morpheus.Schema.TypeKind          (TypeKind (..))
import qualified Data.Morpheus.Schema.Utils.InputValue  as UI (typeName)
import           Data.Morpheus.Schema.Utils.Utils       (Field, InputValue, TypeLib)
import           Data.Morpheus.Types.Core               (EnhancedKey (..))
import           Data.Morpheus.Types.Describer          (EnumOf (..))
import           Data.Morpheus.Types.Error              (MetaValidation, Validation)
import           Data.Morpheus.Types.JSType             (JSType (..))
import           Data.Morpheus.Types.MetaInfo           (Position)
import qualified Data.Morpheus.Types.Query.RawSelection as Raw (RawArguments)
import           Data.Morpheus.Types.Query.Selection    (Argument (..), Arguments)
import           Data.Morpheus.Types.Types              (GQLQueryRoot (..))
import           Data.Text                              (Text)

asGQLError :: MetaValidation a -> Validation a
asGQLError (Left err)    = Left $ argumentError err
asGQLError (Right value) = pure value

-- TODO: Validate other Types , type missmatch
checkArgumentType :: TypeLib -> (Text, Int) -> (Text, Argument) -> Validation (Text, Argument)
checkArgumentType typeLib (typeName, position') (aKey, argument) =
  asGQLError (existsType (position', aKey) typeName typeLib) >>= checkType
  where
    checkType _type =
      case T.kind _type of
        EnumOf ENUM -> validateEnum _type argument >>= \x -> pure (aKey, x)
       -- INPUT_OBJECT is already validated
        _           -> pure (aKey, argument)

validateArgument :: TypeLib -> Position -> Arguments -> InputValue -> Validation (Text, Argument)
validateArgument types position' requestArgs inpValue =
  case lookup (I.name inpValue) requestArgs of
    Nothing ->
      if I.isRequired inpValue
        then Left $ requiredArgument (I.inputValueMeta position' inpValue)
        else pure (key, Argument JSNull position')
    Just (Argument value pos) -> checkArgumentType types (UI.typeName inpValue, pos) (key, Argument value pos)
  where
    key = I.name inpValue

onlyResolveArguments :: GQLQueryRoot -> Position -> Raw.RawArguments -> Validation Arguments
onlyResolveArguments root _ = mapM (replaceVariable root)

checkForUnknownArguments :: Field -> Arguments -> Validation [InputValue]
checkForUnknownArguments field args =
  case differKeys (map argToKey args) fieldKeys of
    []          -> pure $ F.args field
    unknownArgs -> Left $ unknownArguments (F.name field) unknownArgs
  where
    argToKey (key', Argument _ pos) = EnhancedKey key' pos
    fieldKeys = map I.name (F.args field)

resolveArguments :: TypeLib -> GQLQueryRoot -> Field -> Position -> Raw.RawArguments -> Validation Arguments
resolveArguments typeLib root inputs pos args =
  onlyResolveArguments root pos args >>= validateArguments typeLib inputs pos

validateArguments :: TypeLib -> Field -> Position -> Arguments -> Validation Arguments
validateArguments typeLib inputs pos args =
  checkForUnknownArguments inputs args >>= mapM (validateArgument typeLib pos args)
