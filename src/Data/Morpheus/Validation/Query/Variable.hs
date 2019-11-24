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
                                                , FragmentLib
                                                , RawArgument(..)
                                                , RawSelection(..)
                                                , RawSelectionSet
                                                , Selection(..)
                                                , Ref(..)
                                                , Position
                                                , DataType
                                                , DataTypeLib
                                                , lookupInputType
                                                , Variables
                                                , Value(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )
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

allVariableRefs :: FragmentLib -> [RawSelectionSet] -> Validation [Ref]
allVariableRefs fragmentLib = concatMapM (concatMapM searchRefs)
 where
  referencesFromArgument :: (Text, RawArgument) -> [Ref]
  referencesFromArgument (_, RawArgument{}) = []
  referencesFromArgument (_, VariableRef Ref { refName, refPosition }) =
    [Ref refName refPosition]
  -- | search used variables in every arguments
  searchRefs :: (Text, RawSelection) -> Validation [Ref]
  searchRefs (_, RawSelectionSet Selection { selectionArguments, selectionRec })
    = getArgs <$> concatMapM searchRefs selectionRec
   where
    getArgs :: [Ref] -> [Ref]
    getArgs x = concatMap referencesFromArgument selectionArguments <> x
  searchRefs (_, InlineFragment Fragment { fragmentSelection }) =
    concatMapM searchRefs fragmentSelection
  searchRefs (_, RawSelectionField Selection { selectionArguments }) =
    return $ concatMap referencesFromArgument selectionArguments
  searchRefs (_, Spread reference) =
    getFragment reference fragmentLib
      >>= concatMapM searchRefs
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
    allVariableRefs lib [operationSelection] >>= checkUnusedVariables
    mapM (lookupAndValidateValueOnBody typeLib root validationMode)
         operationArgs
 where
  varToKey :: (Text, Variable a) -> Ref
  varToKey (key', Variable { variablePosition }) = Ref key' variablePosition
  --
  checkUnusedVariables :: [Ref] -> Validation ()
  checkUnusedVariables refs = case map varToKey operationArgs \\ refs of
    [] -> pure ()
    unused' ->
      failure $ unusedVariables (getOperationName operationName) unused'

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
    = failure $ uninitializedVariable variablePosition variableType key
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
        Left message -> failure $ variableGotInvalidValue
          key
          (inputErrorMessage message)
          variablePosition
        Right value -> pure (key, value)
