{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Validation.Query.Selection
  ( validateSelectionSet
  ) where

import           Data.Morpheus.Error.Selection                  (cannotQueryField, duplicateQuerySelections,
                                                                 hasNoSubfields, subfieldsNotSelected)
import           Data.Morpheus.Error.Variable                   (unknownType)
import           Data.Morpheus.Types.Internal.AST.Operation     (ValidVariables)
import           Data.Morpheus.Types.Internal.AST.RawSelection  (Fragment (..), FragmentLib, RawSelection (..),
                                                                 RawSelection' (..), RawSelectionSet)
import           Data.Morpheus.Types.Internal.AST.Selection     (Selection (..), SelectionRec (..), SelectionSet)
import           Data.Morpheus.Types.Internal.Base              (EnhancedKey (..))
import           Data.Morpheus.Types.Internal.Data              (DataField (..), DataFullType (..), DataObject,
                                                                 DataType (..), DataTypeLib (..), allDataTypes)
import           Data.Morpheus.Types.Internal.Validation        (Validation)
import           Data.Morpheus.Validation.Internal.Utils        (checkNameCollision, lookupType)
import           Data.Morpheus.Validation.Query.Arguments       (validateArguments)
import           Data.Morpheus.Validation.Query.Fragment        (castFragmentType, resolveSpread)
import           Data.Morpheus.Validation.Query.Utils.Selection (lookupFieldAsSelectionSet, lookupSelectionField,
                                                                 lookupUnionTypes)
import           Data.Text                                      (Text)

checkDuplicatesOn :: DataObject -> SelectionSet -> Validation SelectionSet
checkDuplicatesOn DataType {typeName = name'} keys = checkNameCollision enhancedKeys selError >> pure keys
  where
    selError = duplicateQuerySelections name'
    enhancedKeys = map selToKey keys
    selToKey (key', Selection {selectionPosition = position'}) = EnhancedKey key' position'

clusterUnionSelection ::
     FragmentLib -> Text -> [DataObject] -> (Text, RawSelection) -> Validation ([Fragment], SelectionSet)
clusterUnionSelection fragments type' possibleTypes' = splitFrag
  where
    packFragment fragment = return ([fragment], [])
    typeNames = map typeName possibleTypes'
    splitFrag :: (Text, RawSelection) -> Validation ([Fragment], SelectionSet)
    splitFrag (_, Spread ref) = resolveSpread fragments typeNames ref >>= packFragment
    splitFrag ("__typename", RawSelectionField RawSelection' {rawSelectionPosition = position'}) =
      return
        ( []
        , [ ( "__typename"
            , Selection {selectionRec = SelectionField, selectionArguments = [], selectionPosition = position'})
          ])
    splitFrag (key, RawSelectionSet RawSelection' {rawSelectionPosition}) =
      Left $ cannotQueryField key type' rawSelectionPosition
    splitFrag (key, RawSelectionField RawSelection' {rawSelectionPosition}) =
      Left $ cannotQueryField key type' rawSelectionPosition
    splitFrag (key', RawAlias {rawAliasPosition = position'}) = Left $ cannotQueryField key' type' position'
    splitFrag (_, InlineFragment fragment') =
      castFragmentType Nothing (fragmentPosition fragment') typeNames fragment' >>= packFragment

categorizeTypes :: [DataObject] -> [Fragment] -> [(DataObject, [Fragment])]
categorizeTypes types fragments = filter notEmpty $ map categorizeType types
  where
    notEmpty = (0 /=) . length . snd
    categorizeType :: DataObject -> (DataObject, [Fragment])
    categorizeType datatype = (datatype, filter matches fragments)
      where
        matches fragment = fragmentType fragment == typeName datatype

flatTuple :: [([a], [b])] -> ([a], [b])
flatTuple list' = (concatMap fst list', concatMap snd list')
 {-
    - all Variable and Fragment references will be: resolved and validated
    - unionTypes: will be clustered under type names
      ...A on T1 {<SelectionA>}
      ...B on T2 {<SelectionB>}
      ...C on T2 {<SelectionC>}
      will be become : [
          ("T1",[<SelectionA>]),
          ("T2",[<SelectionB>,<SelectionC>])
      ]
 -}

validateSelectionSet ::
     DataTypeLib -> FragmentLib -> Text -> ValidVariables -> DataObject -> RawSelectionSet -> Validation SelectionSet
validateSelectionSet lib fragments' operatorName variables = __validate
  where
    __validate dataType'@DataType {typeName = typeName'} selectionSet' =
      concat <$> mapM validateSelection selectionSet' >>= checkDuplicatesOn dataType'
      where
        validateFragment Fragment {fragmentSelection = selection'} = __validate dataType' selection'
        {-
            get dataField and validated arguments for RawSelection
        -}
        getValidationData key RawSelection' {rawSelectionArguments, rawSelectionPosition} = do
          selectionField <- lookupSelectionField rawSelectionPosition key dataType'
          -- validate field Argument -----
          arguments <-
            validateArguments
              lib
              operatorName
              variables
              (key, selectionField)
              rawSelectionPosition
              rawSelectionArguments
          -- check field Type existence  -----
          fieldDataType <-
            lookupType
              (unknownType (fieldType selectionField) rawSelectionPosition)
              (allDataTypes lib)
              (fieldType selectionField)
          return (selectionField, fieldDataType, arguments)
        -- validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
        --
        validateSelection :: (Text, RawSelection) -> Validation SelectionSet
        validateSelection (key', RawAlias {rawAliasSelection = rawSelection', rawAliasPosition = position'}) =
          fmap processSingleSelection <$> validateSelection rawSelection'
          where
            processSingleSelection (selKey', selection') =
              ( key'
              , selection'
                  { selectionRec = SelectionAlias {aliasFieldName = selKey', aliasSelection = selectionRec selection'}
                  , selectionPosition = position'
                  })
        validateSelection (key', RawSelectionSet fullRawSelection'@RawSelection' { rawSelectionRec = rawSelectors
                                                                                 , rawSelectionPosition = position'
                                                                                 }) = do
          (dataField', dataType, arguments) <- getValidationData key' fullRawSelection'
          case dataType of
            Union _ -> do
              (categories, __typename) <- clusterTypes
              mapM (validateCluster __typename) categories >>= returnSelection arguments . UnionSelection
              where clusterTypes = do
                      unionTypes <- lookupUnionTypes position' key' lib dataField'
                      (spreads, __typename) <-
                        flatTuple <$> mapM (clusterUnionSelection fragments' typeName' unionTypes) rawSelectors
                      return (categorizeTypes unionTypes spreads, __typename)
                    --
                    --    second arguments will be added to every selection cluster
                    validateCluster :: SelectionSet -> (DataObject, [Fragment]) -> Validation (Text, SelectionSet)
                    validateCluster sysSelection' (type', frags') = do
                      selection' <- __validate type' (concatMap fragmentSelection frags')
                      return (typeName type', sysSelection' ++ selection')
            OutputObject _ -> do
              fieldType' <- lookupFieldAsSelectionSet position' key' lib dataField'
              __validate fieldType' rawSelectors >>= returnSelection arguments . SelectionSet
                 -- DataFullType
                       --   = Leaf DataLeaf
                       --   | InputObject DataInputObject
                       --   | InputUnion DataUnion
            _ -> Left $ hasNoSubfields key' (fieldType dataField') position'
          where
            returnSelection arguments' selection' =
              pure
                [ ( key'
                  , Selection
                      {selectionArguments = arguments', selectionRec = selection', selectionPosition = position'})
                ]
        validateSelection (key', RawSelectionField fullRawSelection'@RawSelection' {rawSelectionPosition}) = do
          (dataField, datatype, arguments') <- getValidationData key' fullRawSelection'
          isLeaf datatype dataField
          pure
            [ ( key'
              , Selection
                  { selectionArguments = arguments'
                  , selectionRec = SelectionField
                  , selectionPosition = rawSelectionPosition
                  })
            ]
          where
            isLeaf (Leaf _) _              = Right ()
            isLeaf _ DataField {fieldType} = Left $ subfieldsNotSelected key' fieldType rawSelectionPosition
        validateSelection (_, Spread reference') = resolveSpread fragments' [typeName'] reference' >>= validateFragment
        validateSelection (_, InlineFragment fragment') =
          castFragmentType Nothing (fragmentPosition fragment') [typeName'] fragment' >>= validateFragment
