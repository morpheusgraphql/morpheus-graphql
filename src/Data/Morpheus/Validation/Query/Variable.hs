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
import           Data.Morpheus.Types.Internal.AST.Operation
                                                ( DefaultValue
                                                , Operation(..)
                                                , RawOperation
                                                , ValidVariables
                                                , Variable(..)
                                                , getOperationName
                                                )
import           Data.Morpheus.Types.Internal.AST.Selection
                                                ( Fragment(..)
                                                , FragmentLib
                                                , RawArgument(..)
                                                , RawSelection(..)
                                                , RawSelectionSet
                                                , Selection(..)
                                                )
import           Data.Morpheus.Types.Internal.Base
                                                ( Reference(..)
                                                , Position
                                                )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( DataType
                                                , DataTypeLib
                                                , lookupInputType
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation )
import           Data.Morpheus.Types.Internal.AST.Value
                                                ( Value(..) )
import           Data.Morpheus.Types.Types      ( Variables )
import           Data.Morpheus.Validation.Internal.Utils
                                                ( VALIDATION_MODE(..) )
import           Data.Morpheus.Validation.Internal.Value
                                                ( validateInputValue )
import           Data.Morpheus.Validation.Query.Fragment
                                                ( getFragment )

getVariableType :: Text -> Position -> DataTypeLib -> Validation DataType
getVariableType type' position' lib' = lookupInputType type' lib' error'
  where error' = unknownType type' position'

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

allVariableReferences
  :: FragmentLib -> [RawSelectionSet] -> Validation [Reference]
allVariableReferences fragmentLib = concatMapM (concatMapM searchReferences)
 where
  referencesFromArgument :: (Text, RawArgument) -> [Reference]
  referencesFromArgument (_, RawArgument{}) = []
  referencesFromArgument (_, VariableReference Reference { refName, refPosition })
    = [Reference refName refPosition]
  -- | search used variables in every arguments
  searchReferences :: (Text, RawSelection) -> Validation [Reference]
  searchReferences (_, RawSelectionSet Selection { selectionArguments, selectionRec })
    = getArgs <$> concatMapM searchReferences selectionRec
   where
    getArgs :: [Reference] -> [Reference]
    getArgs x = concatMap referencesFromArgument selectionArguments <> x
  searchReferences (_, InlineFragment Fragment { fragmentSelection }) =
    concatMapM searchReferences fragmentSelection
  searchReferences (_, RawSelectionField Selection { selectionArguments }) =
    return $ concatMap referencesFromArgument selectionArguments
  searchReferences (_, Spread reference) =
    getFragment reference fragmentLib
      >>= concatMapM searchReferences
      .   fragmentSelection

resolveOperationVariables
  :: DataTypeLib
  -> FragmentLib
  -> Variables
  -> VALIDATION_MODE
  -> RawOperation
  -> Validation ValidVariables
resolveOperationVariables typeLib lib root validationMode Operation { operationName, operationSelection, operationArgs }
  = do
    allVariableReferences lib [operationSelection] >>= checkUnusedVariables
    mapM (lookupAndValidateValueOnBody typeLib root validationMode)
         operationArgs
 where
  varToKey :: (Text, Variable a) -> Reference
  varToKey (key', Variable { variablePosition }) =
    Reference key' variablePosition
  --
  checkUnusedVariables :: [Reference] -> Validation ()
  checkUnusedVariables refs = case map varToKey operationArgs \\ refs of
    []      -> pure ()
    unused' -> Left $ unusedVariables (getOperationName operationName) unused'

lookupAndValidateValueOnBody
  :: DataTypeLib
  -> Variables
  -> VALIDATION_MODE
  -> (Text, Variable DefaultValue)
  -> Validation (Text, Variable Value)
lookupAndValidateValueOnBody typeLib bodyVariables validationMode (key, var@Variable { variableType, variablePosition, isVariableRequired, variableTypeWrappers, variableValue = defaultValue })
  = toVariable
    <$> (   getVariableType variableType variablePosition typeLib
        >>= checkType getVariable defaultValue
        )
 where
  toVariable (varKey, variableValue) = (varKey, var { variableValue })
  getVariable = M.lookup key bodyVariables
  ------------------------------------------------------------------
  checkType (Just variable) Nothing varType = validator varType variable
  checkType (Just variable) (Just defValue) varType =
    validator varType defValue >> validator varType variable
  checkType Nothing (Just defValue) varType = validator varType defValue
  checkType Nothing Nothing varType
    | validationMode /= WITHOUT_VARIABLES && isVariableRequired
    = Left $ uninitializedVariable variablePosition variableType key
    | otherwise
    = returnNull
   where
    returnNull =
      maybe (pure (key, Null)) (validator varType) (M.lookup key bodyVariables)
  -----------------------------------------------------------------------------------------------
  validator varType varValue =
    case
        validateInputValue typeLib
                           []
                           variableTypeWrappers
                           varType
                           (key, varValue)
      of
        Left message -> Left $ variableGotInvalidValue
          key
          (inputErrorMessage message)
          variablePosition
        Right value -> pure (key, value)
