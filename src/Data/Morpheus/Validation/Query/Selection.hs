{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Validation.Query.Selection
  ( validateSelectionSet
  )
where


import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )

-- MORPHEUS
import           Data.Morpheus.Error.Selection  ( cannotQueryField
                                                , duplicateQuerySelections
                                                , hasNoSubfields
                                                , subfieldsNotSelected
                                                )
import           Data.Morpheus.Error.Variable   ( unknownType )
import           Data.Morpheus.Types.Internal.AST
                                                ( ValidVariables
                                                , Selection(..)
                                                , SelectionRec(..)
                                                , SelectionSet
                                                , Fragment(..)
                                                , FragmentLib
                                                , RawSelection(..)
                                                , RawSelectionSet
                                                , DataField(..)
                                                , Ref(..)
                                                , DataObject
                                                , DataTypeContent(..)
                                                , DataType(..)
                                                , DataTypeLib(..)
                                                , TypeAlias(..)
                                                , Name
                                                , allDataTypes
                                                , isEntNode
                                                , lookupFieldAsSelectionSet
                                                , lookupSelectionField
                                                , lookupType
                                                , lookupUnionTypes
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )
import           Data.Morpheus.Validation.Internal.Utils
                                                ( checkNameCollision )
import           Data.Morpheus.Validation.Query.Arguments
                                                ( validateArguments )
import           Data.Morpheus.Validation.Query.Fragment
                                                ( castFragmentType
                                                , resolveSpread
                                                )

checkDuplicatesOn :: Name -> SelectionSet -> Validation SelectionSet
checkDuplicatesOn typeName keys = checkNameCollision enhancedKeys selError
  >> pure keys
 where
  selError     = duplicateQuerySelections typeName
  enhancedKeys = map selToKey keys
  selToKey (key, Selection { selectionPosition = position', selectionAlias }) =
    Ref (fromMaybe key selectionAlias) position'

clusterUnionSelection
  :: FragmentLib
  -> Text
  -> [Name]
  -> (Text, RawSelection)
  -> Validation ([Fragment], SelectionSet)
clusterUnionSelection fragments type' typeNames = splitFrag
 where
  packFragment fragment = return ([fragment], [])
  splitFrag :: (Text, RawSelection) -> Validation ([Fragment], SelectionSet)
  splitFrag (_, Spread ref) =
    resolveSpread fragments typeNames ref >>= packFragment
  splitFrag ("__typename", RawSelectionField selection) = pure
    ( []
    , [ ( "__typename"
        , selection { selectionArguments = [], selectionRec = SelectionField }
        )
      ]
    )
  splitFrag (key, RawSelectionSet Selection { selectionPosition }) =
    failure $ cannotQueryField key type' selectionPosition
  splitFrag (key, RawSelectionField Selection { selectionPosition }) =
    failure $ cannotQueryField key type' selectionPosition
--  splitFrag (key', RawAlias {rawAliasPosition = position'}) = failure $ cannotQueryField key' type' position'
  splitFrag (_, InlineFragment fragment') =
    castFragmentType Nothing (fragmentPosition fragment') typeNames fragment'
      >>= packFragment

categorizeTypes
  :: [(Name, DataObject)] -> [Fragment] -> [((Name, DataObject), [Fragment])]
categorizeTypes types fragments = filter notEmpty $ map categorizeType types
 where
  notEmpty = (0 /=) . length . snd
  categorizeType :: (Name, DataObject) -> ((Name, DataObject), [Fragment])
  categorizeType datatype = (datatype, filter matches fragments)
    where matches fragment = fragmentType fragment == fst datatype

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

validateSelectionSet
  :: DataTypeLib
  -> FragmentLib
  -> Text
  -> ValidVariables
  -> (Name, DataObject)
  -> RawSelectionSet
  -> Validation SelectionSet
validateSelectionSet lib fragments' operatorName variables = __validate
 where
  __validate :: (Name, DataObject) -> RawSelectionSet -> Validation SelectionSet
  __validate dataType@(typeName, objectFields) selectionSet =
    concat
    <$> mapM validateSelection selectionSet
    >>= checkDuplicatesOn typeName
   where
    validateFragment Fragment { fragmentSelection = selection' } =
      __validate dataType selection'
    {-
            get dataField and validated arguments for RawSelection
        -}
    --getValidationData
    --  :: Name -> RawSelection -> (DataField, DataTypeContent, Arguments)
    getValidationData key Selection { selectionArguments, selectionPosition } =
      do
        selectionField <- lookupSelectionField selectionPosition
                                               key
                                               typeName
                                               objectFields
        -- validate field Argument -----
        arguments <- validateArguments lib
                                       operatorName
                                       variables
                                       (key, selectionField)
                                       selectionPosition
                                       selectionArguments
        -- check field Type existence  -----
        fieldDataType <- lookupType
          (unknownType (aliasTyCon $fieldType selectionField) selectionPosition)
          (allDataTypes lib)
          (aliasTyCon $ fieldType selectionField)
        return (selectionField, fieldDataType, arguments)
    -- validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
    --
    validateSelection :: (Text, RawSelection) -> Validation SelectionSet
    validateSelection (key', RawSelectionSet fullRawSelection@Selection { selectionRec = rawSelection, selectionPosition })
      = do
        (dataField, dataType, arguments) <- getValidationData key'
                                                              fullRawSelection
        case typeContent dataType of
          DataUnion _ -> do
            (categories, __typename) <- clusterTypes
            mapM (validateCluster __typename) categories
              >>= returnSelection arguments
              .   UnionSelection
           where
            clusterTypes = do
              unionTypes <- lookupUnionTypes selectionPosition
                                             key'
                                             lib
                                             dataField
              (spreads, __typename) <-
                flatTuple
                  <$> mapM
                        (   clusterUnionSelection fragments' typeName
                        $   fst
                        <$> unionTypes
                        )
                        rawSelection
              return (categorizeTypes unionTypes spreads, __typename)
            --
            --    second arguments will be added to every selection cluster
            validateCluster
              :: SelectionSet
              -> ((Name, DataObject), [Fragment])
              -> Validation (Text, SelectionSet)
            validateCluster sysSelection' (type', frags') = do
              selection' <- __validate type'
                                       (concatMap fragmentSelection frags')
              return (fst type', sysSelection' ++ selection')
          DataObject _ -> do
            fieldType' <- lookupFieldAsSelectionSet selectionPosition
                                                    key'
                                                    lib
                                                    dataField
            __validate fieldType' rawSelection
              >>= returnSelection arguments
              .   SelectionSet
          _ -> failure $ hasNoSubfields key'
                                        (aliasTyCon $fieldType dataField)
                                        selectionPosition
     where
      returnSelection selectionArguments selectionRec =
        pure [(key', fullRawSelection { selectionArguments, selectionRec })]
    validateSelection (key, RawSelectionField rawSelection@Selection { selectionPosition })
      = do
        (dataField, datatype, selectionArguments) <- getValidationData
          key
          rawSelection
        isLeaf (typeContent datatype) dataField
        pure
          [ ( key
            , rawSelection { selectionArguments, selectionRec = SelectionField }
            )
          ]
     where
      isLeaf dataType DataField { fieldType = TypeAlias { aliasTyCon } }
        | isEntNode dataType = pure ()
        | otherwise = failure
        $ subfieldsNotSelected key aliasTyCon selectionPosition
    validateSelection (_, Spread reference') =
      resolveSpread fragments' [typeName] reference' >>= validateFragment
    validateSelection (_, InlineFragment fragment') =
      castFragmentType Nothing (fragmentPosition fragment') [typeName] fragment'
        >>= validateFragment
