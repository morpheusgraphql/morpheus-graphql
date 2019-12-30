{-# LANGUAGE GADTs #-}
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
                                                , SelectionContent(..)
                                                , ValidSelection
                                                , ValidSelectionSet
                                                , Fragment(..)
                                                , FragmentLib
                                                , RawSelection
                                                , RawSelectionSet
                                                , DataField(..)
                                                , Ref(..)
                                                , DataObject
                                                , DataTypeContent(..)
                                                , DataType(..)
                                                , Schema(..)
                                                , TypeRef(..)
                                                , Name
                                                , allDataTypes
                                                , isEntNode
                                                , lookupFieldAsSelectionSet
                                                , lookupSelectionField
                                                , lookupType
                                                , lookupUnionTypes
                                                , checkNameCollision
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Failure(..)
                                                )
import           Data.Morpheus.Validation.Query.Arguments
                                                ( validateArguments )
import           Data.Morpheus.Validation.Query.Fragment
                                                ( castFragmentType
                                                , resolveSpread
                                                )

checkDuplicatesOn :: Name -> ValidSelectionSet -> Validation ValidSelectionSet
checkDuplicatesOn typeName keys = checkNameCollision enhancedKeys selError
  >> pure keys
 where
  selError     = duplicateQuerySelections typeName
  enhancedKeys = map selToKey keys
  selToKey :: (Name, ValidSelection) -> Ref
  selToKey (key, Selection { selectionPosition = position', selectionAlias }) =
    Ref (fromMaybe key selectionAlias) position'

clusterUnionSelection
  :: FragmentLib
  -> Text
  -> [Name]
  -> (Text, RawSelection)
  -> Validation ([Fragment], ValidSelectionSet)
clusterUnionSelection fragments type' typeNames = splitFrag
 where
  packFragment fragment = return ([fragment], [])
  splitFrag
    :: (Text, RawSelection) -> Validation ([Fragment], ValidSelectionSet)
  splitFrag (_, Spread ref) =
    resolveSpread fragments typeNames ref >>= packFragment
  splitFrag ("__typename", selection@Selection { selectionContent = SelectionField })
    = pure
      ( []
      , [ ( "__typename"
          , selection { selectionArguments = [], selectionContent = SelectionField }
          )
        ]
      )
  splitFrag (key, Selection { selectionPosition }) =
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
  :: Schema
  -> FragmentLib
  -> Text
  -> ValidVariables
  -> (Name, DataObject)
  -> RawSelectionSet
  -> Validation ValidSelectionSet
validateSelectionSet lib fragments' operatorName variables = __validate
 where
  __validate
    :: (Name, DataObject) -> RawSelectionSet -> Validation ValidSelectionSet
  __validate dataType@(typeName, objectFields) selectionSet =
    concat
    <$> mapM validateSelection selectionSet
    >>= checkDuplicatesOn typeName
   where
    -- getValidationData :: Name -> ValidSelection -> (DataField, DataTypeContent, ValidArguments)
    getValidationData key (selectionArguments, selectionPosition) = do
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
        (unknownType (typeConName $fieldType selectionField) selectionPosition) lib
        (typeConName $ fieldType selectionField)
      return (selectionField, fieldDataType, arguments)
    -- validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
    --
    validateSelection :: (Text, RawSelection) -> Validation ValidSelectionSet
    validateSelection ("__typename", sel@Selection { selectionArguments = [], selectionContent = SelectionField}) = 
      pure [("__typename", sel { selectionArguments = [], selectionContent = SelectionField })]
    validateSelection (key', fullRawSelection@Selection { selectionArguments = selArgs, selectionContent = SelectionSet rawSelection, selectionPosition })
      = do
        (dataField, datatype, arguments) <- getValidationData
          key'
          (selArgs, selectionPosition)
        case typeContent datatype of
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
              :: ValidSelectionSet
              -> ((Name, DataObject), [Fragment])
              -> Validation (Text, ValidSelectionSet)
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
                                        (typeConName $fieldType dataField)
                                        selectionPosition
     where
      returnSelection selectionArguments selectionContent =
        pure [(key', fullRawSelection { selectionArguments, selectionContent })]
    validateSelection (key, rawSelection@Selection { selectionArguments = selArgs, selectionPosition, selectionContent = SelectionField })
      = do
        (dataField, datatype, selectionArguments) <- getValidationData
          key
          (selArgs, selectionPosition)
        isLeaf (typeContent datatype) dataField
        pure
          [ ( key
            , rawSelection { selectionArguments, selectionContent = SelectionField }
            )
          ]
     where
      isLeaf datatype DataField { fieldType = TypeRef { typeConName } }
        | isEntNode datatype = pure ()
        | otherwise = failure
        $ subfieldsNotSelected key typeConName selectionPosition
    validateSelection (_, Spread reference') =
      resolveSpread fragments' [typeName] reference' >>= validateFragment
    validateSelection (_, InlineFragment fragment') =
      castFragmentType Nothing (fragmentPosition fragment') [typeName] fragment'
        >>= validateFragment
    validateFragment Fragment { fragmentSelection } = __validate dataType fragmentSelection
