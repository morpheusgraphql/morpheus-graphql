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
                                                , FragmentLib
                                                , Argument(..)
                                                , RawArgument
                                                , Selection(..)
                                                , SelectionRec(..)
                                                , RawSelection
                                                , RawSelectionSet
                                                , Ref(..)
                                                , Position
                                                , DataType
                                                , DataTypeLib
                                                , lookupInputType
                                                , Variables
                                                , Value(..)
                                                , ValidValue
                                                , RawValue
                                                , ResolvedValue
                                                , Name
                                                , RESOLVED
                                                , VALID
                                                , RAW
                                                , VariableContent(..)
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


class ExploreRefs a where
  exploreRefs :: a -> [Ref]

instance ExploreRefs RawValue where
  exploreRefs (VariableValue ref   ) = [ref]
  exploreRefs (Object        fields) = concatMap (exploreRefs . snd) fields
  exploreRefs (List          ls    ) = concatMap exploreRefs ls
  exploreRefs _                      = []

instance ExploreRefs (Text, RawArgument) where
  exploreRefs (_, Argument { argumentValue }) = exploreRefs argumentValue

allVariableRefs :: FragmentLib -> [RawSelectionSet] -> Validation [Ref]
allVariableRefs fragmentLib = concatMapM (concatMapM searchRefs)
 where

  -- | search used variables in every arguments
  searchRefs :: (Text, RawSelection) -> Validation [Ref]
  searchRefs (_, Selection { selectionArguments, selectionRec = SelectionField })
    = return $ concatMap exploreRefs selectionArguments
  searchRefs (_, Selection { selectionArguments, selectionRec = SelectionSet selSet })
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
  -> (Text, Variable RAW)
  -> Validation (Text, Variable VALID)
lookupAndValidateValueOnBody typeLib bodyVariables validationMode (key, var@Variable { variableType, variablePosition, isVariableRequired, variableTypeWrappers, variableValue = DefaultValue defaultValue })
  = toVariable
    <$> (   getVariableType variableType variablePosition typeLib
        >>= checkType getVariable defaultValue
        )
 where
  toVariable (varKey, x) =
    (varKey, var { variableValue = ValidVariableValue x })
  getVariable :: Maybe ResolvedValue
  getVariable = M.lookup key bodyVariables
  ------------------------------------------------------------------
  -- checkType :: 
  checkType
    :: Maybe ResolvedValue
    -> DefaultValue
    -> DataType
    -> Validation (Name, ValidValue)
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
  validator :: DataType -> ResolvedValue -> Validation (Name, ValidValue)
  validator varType varValue =
    case
        validateInputValue typeLib
                           []
                           variableTypeWrappers
                           varType
                           (key, varValue)
      of
        Left message -> failure $ case inputErrorMessage message of
          Left errors -> errors
          Right errMessage ->
            variableGotInvalidValue key errMessage variablePosition
        Right value -> pure (key, value)
