{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Query.Variable
  ( resolveOperationVariables
  )
where

import           Data.List                      ( (\\) )
import qualified Data.Map                      as M
                                                ( lookup )
import           Data.Maybe                     ( maybe )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )

--- MORPHEUS
import           Data.Morpheus.Error.Input      ( inputErrorMessage )
import           Data.Morpheus.Error.Variable   ( uninitializedVariable
                                                , unknownType
                                                , unusedVariables
                                                , variableGotInvalidValue
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( DefaultValue
                                                , Operation(..)
                                                , RawOperation
                                                , ValidVariables
                                                , Variable(..)
                                                , getOperationName
                                                , Fragment(..)
                                                , Fragments
                                                , Argument(..)
                                                , RawArgument
                                                , Selection(..)
                                                , SelectionContent(..)
                                                , RawSelection
                                                , RawSelectionSet
                                                , Ref(..)
                                                , Position
                                                , TypeDefinition
                                                , Schema
                                                , lookupInputType
                                                , Variables
                                                , Value(..)
                                                , ValidValue
                                                , RawValue
                                                , ResolvedValue
                                                , Name
                                                , VALID
                                                , RAW
                                                , VariableContent(..)
                                                , isNullable
                                                , TypeRef(..)
                                                , VALIDATION_MODE(..)
                                                , ObjectEntry(..)
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                (Listable(..))
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )
import           Data.Morpheus.Validation.Internal.Value
                                                ( validateInputValue )
import           Data.Morpheus.Validation.Query.Fragment
                                                ( getFragment )

getVariableType :: Text -> Position -> Schema -> Validation TypeDefinition
getVariableType type' position' lib' = lookupInputType type' lib' error'
  where error' = unknownType type' position'

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . traverse f


class ExploreRefs a where
  exploreRefs :: a -> [Ref]

instance ExploreRefs RawValue where
  exploreRefs (VariableValue ref   ) = [ref]
  exploreRefs (Object        fields) = concatMap (exploreRefs . entryValue) fields
  exploreRefs (List          ls    ) = concatMap exploreRefs ls
  exploreRefs _                      = []

instance ExploreRefs RawArgument where
  exploreRefs = exploreRefs . argumentValue

allVariableRefs :: Fragments -> [RawSelectionSet] -> Validation [Ref]
allVariableRefs fragmentLib = concatMapM (concatMapM searchRefs)
 where

  -- | search used variables in every arguments
  searchRefs :: (Text, RawSelection) -> Validation [Ref]
  searchRefs (_, Selection { selectionArguments, selectionContent = SelectionField })
    = return $ concatMap exploreRefs selectionArguments
  searchRefs (_, Selection { selectionArguments, selectionContent = SelectionSet selSet })
    = getArgs <$> concatMapM searchRefs selSet
   where
    getArgs :: [Ref] -> [Ref]
    getArgs x = concatMap exploreRefs selectionArguments <> x
  searchRefs (_, InlineFragment Fragment { fragmentSelection }) =
    concatMapM searchRefs fragmentSelection
  searchRefs (_, Spread reference) =
    getFragment reference fragmentLib
      >>= concatMapM searchRefs
      .   fragmentSelection

resolveOperationVariables
  :: Schema
  -> Fragments
  -> Variables
  -> VALIDATION_MODE
  -> RawOperation
  -> Validation ValidVariables
resolveOperationVariables typeLib lib root validationMode Operation { operationName, operationSelection, operationArguments }
  = do
    allVariableRefs lib [operationSelection] >>= checkUnusedVariables
    traverse (lookupAndValidateValueOnBody typeLib root validationMode) operationArguments
 where
  varToKey :: Variable a -> Ref
  varToKey Variable { variableName, variablePosition } = Ref variableName variablePosition
  --
  checkUnusedVariables :: [Ref] -> Validation ()
  checkUnusedVariables refs = case map varToKey (toList operationArguments) \\ refs of
    [] -> pure ()
    unused' ->
      failure $ unusedVariables (getOperationName operationName) unused'

lookupAndValidateValueOnBody
  :: Schema
  -> Variables
  -> VALIDATION_MODE
  -> Variable RAW
  -> Validation (Variable VALID)
lookupAndValidateValueOnBody 
  typeLib bodyVariables validationMode 
  var@Variable { 
      variableName,
      variableType, 
      variablePosition, 
      variableValue = DefaultValue defaultValue 
    }
  = toVariable
    <$> (   getVariableType (typeConName variableType) variablePosition typeLib
        >>= checkType getVariable defaultValue
        )
 where
  toVariable x = var { variableValue = ValidVariableValue x }
  getVariable :: Maybe ResolvedValue
  getVariable = M.lookup variableName bodyVariables
  ------------------------------------------------------------------
  -- checkType :: 
  checkType
    :: Maybe ResolvedValue
    -> DefaultValue
    -> TypeDefinition
    -> Validation ValidValue
  checkType (Just variable) Nothing varType = validator varType variable
  checkType (Just variable) (Just defValue) varType =
    validator varType defValue >> validator varType variable
  checkType Nothing (Just defValue) varType = validator varType defValue
  checkType Nothing Nothing varType
    | validationMode /= WITHOUT_VARIABLES && not (isNullable variableType)
    = failure
      $ uninitializedVariable variablePosition (typeConName variableType) variableName
    | otherwise
    = returnNull
   where
    returnNull =
      maybe (pure Null) (validator varType) (M.lookup variableName bodyVariables)
  -----------------------------------------------------------------------------------------------
  validator :: TypeDefinition -> ResolvedValue -> Validation ValidValue
  validator varType varValue =
    case
        validateInputValue typeLib
                           []
                           (typeWrappers variableType)
                           varType
                           (variableName, varValue)
      of
        Left message -> failure $ case inputErrorMessage message of
          Left errors -> errors
          Right errMessage ->
            variableGotInvalidValue variableName errMessage variablePosition
        Right value -> pure value
