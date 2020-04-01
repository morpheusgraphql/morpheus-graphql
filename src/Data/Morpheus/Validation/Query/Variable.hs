{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Query.Variable
  ( resolveOperationVariables
  )
where

import           Control.Monad                  ((>=>))
import           Data.List                      ( (\\) )
import qualified Data.Map                      as M
                                                ( lookup )
import           Data.Maybe                     ( maybe )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )

--- MORPHEUS
import           Data.Morpheus.Error.Input      ( inputErrorMessage )
import           Data.Morpheus.Error.Variable   ( uninitializedVariable
                                                , unusedVariables
                                                , variableGotInvalidValue
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( DefaultValue
                                                , Operation(..)
                                                , ValidVariables
                                                , Variable(..)
                                                , getOperationName
                                                , Fragment(..)
                                                , Fragments
                                                , Argument(..)
                                                , Selection(..)
                                                , SelectionContent(..)
                                                , SelectionSet
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
                                                , VALID
                                                , RAW
                                                , VariableContent(..)
                                                , isNullable
                                                , TypeRef(..)
                                                , VALIDATION_MODE(..)
                                                , ObjectEntry(..)
                                                , coerceInputType
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( Listable(..)
                                                , selectKnown
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )
import           Data.Morpheus.Validation.Internal.Value
                                                ( validateInputValue )

getVariableType :: Text -> Position -> Schema -> Validation TypeDefinition
getVariableType tyName position 
  = selectKnown (Ref tyName position)
  >=> coerceInputType

class ExploreRefs a where
  exploreRefs :: a -> [Ref]

instance ExploreRefs RawValue where
  exploreRefs (VariableValue ref   ) = [ref]
  exploreRefs (Object        fields) = concatMap (exploreRefs . entryValue) fields
  exploreRefs (List          ls    ) = concatMap exploreRefs ls
  exploreRefs _                      = []

instance ExploreRefs (Argument RAW) where
  exploreRefs = exploreRefs . argumentValue

mapSelection :: (Selection RAW -> Validation [b]) -> SelectionSet RAW -> Validation [b]
mapSelection f = fmap concat . traverse f

allVariableRefs :: Fragments -> [SelectionSet RAW] -> Validation [Ref]
allVariableRefs fragmentLib = fmap concat . traverse (mapSelection searchRefs) 
 where
  -- | search used variables in every arguments
  searchRefs :: Selection RAW -> Validation [Ref]
  searchRefs Selection { selectionArguments, selectionContent = SelectionField }
    = return $ concatMap exploreRefs selectionArguments
  searchRefs Selection { selectionArguments, selectionContent = SelectionSet selSet }
    = getArgs <$> mapSelection searchRefs selSet
   where
    getArgs :: [Ref] -> [Ref]
    getArgs x = concatMap exploreRefs selectionArguments <> x
  searchRefs (InlineFragment Fragment { fragmentSelection })
    = mapSelection searchRefs fragmentSelection
  searchRefs (Spread reference)
    = selectKnown reference fragmentLib
      >>= mapSelection searchRefs
      .   fragmentSelection

resolveOperationVariables
  :: Schema
  -> Fragments
  -> Variables
  -> VALIDATION_MODE
  -> Operation RAW
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
