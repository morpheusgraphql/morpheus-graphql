{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Validation.Query.Variable
  ( resolveOperationVariables
  )
where


import qualified Data.Map                      as M
                                                ( lookup )
import           Data.Maybe                     ( maybe )
import           Data.Semigroup                 ( (<>) )

--- MORPHEUS
import           Data.Morpheus.Error.Variable   ( uninitializedVariable)
import           Data.Morpheus.Types.Internal.AST
                                                ( DefaultValue
                                                , Operation(..)
                                                , Variable(..)
                                                , VariableDefinitions
                                                , Fragment(..)
                                                , Argument(..)
                                                , Selection(..)
                                                , SelectionContent(..)
                                                , SelectionSet
                                                , Ref(..)
                                                , TypeDefinition
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
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( Listable(..)
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( BaseValidator
                                                , askSchema
                                                , askFragments
                                                , selectKnown
                                                , constraint
                                                , Constraint(..)
                                                , withScopePosition
                                                , startInput
                                                , InputSource(..)
                                                , checkUnused
                                                )
import           Data.Morpheus.Validation.Internal.Value
                                                ( validateInput )

class ExploreRefs a where
  exploreRefs :: a -> [Ref]

instance ExploreRefs RawValue where
  exploreRefs (VariableValue ref   ) = [ref]
  exploreRefs (Object        fields) = concatMap (exploreRefs . entryValue) fields
  exploreRefs (List          ls    ) = concatMap exploreRefs ls
  exploreRefs _                      = []

instance ExploreRefs (Argument RAW) where
  exploreRefs = exploreRefs . argumentValue

mapSelection :: (Selection RAW -> BaseValidator [b]) -> SelectionSet RAW -> BaseValidator [b]
mapSelection f = fmap concat . traverse f

allVariableRefs :: [SelectionSet RAW] -> BaseValidator [Ref]
allVariableRefs = fmap concat . traverse (mapSelection searchRefs) 
 where
  -- | search used variables in every arguments
  searchRefs :: Selection RAW -> BaseValidator [Ref]
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
    = askFragments
      >>= selectKnown reference
      >>= mapSelection searchRefs
      .   fragmentSelection

resolveOperationVariables
  :: Variables
  -> VALIDATION_MODE
  -> Operation RAW
  -> BaseValidator (VariableDefinitions VALID)
resolveOperationVariables 
    root 
    validationMode 
    Operation 
      { operationSelection
      , operationArguments 
      }
  = checkUnusedVariables *>
    traverse (lookupAndValidateValueOnBody root validationMode) operationArguments
 where
  checkUnusedVariables :: BaseValidator ()
  checkUnusedVariables = do
    uses <- allVariableRefs [operationSelection]
    checkUnused uses (toList operationArguments)

lookupAndValidateValueOnBody
  :: Variables
  -> VALIDATION_MODE
  -> Variable RAW
  -> BaseValidator (Variable VALID)
lookupAndValidateValueOnBody 
  bodyVariables 
  validationMode 
  var@Variable { 
      variableName,
      variableType, 
      variablePosition, 
      variableValue = DefaultValue defaultValue 
    }
  = withScopePosition variablePosition 
      $ toVariable
      <$> ( askSchema
          >>= selectKnown (Ref (typeConName variableType) variablePosition) 
          >>= constraint INPUT var 
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
    -> BaseValidator ValidValue
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
  validator :: TypeDefinition -> ResolvedValue -> BaseValidator ValidValue
  validator varType varValue 
    = startInput (SourceVariable var) 
        $ validateInput
          (typeWrappers variableType)
          varType
          (ObjectEntry variableName varValue)