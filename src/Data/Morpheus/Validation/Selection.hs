{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Validation.Selection
  ( validateSelectionSet
  ) where

import           Data.Morpheus.Error.Selection                 (cannotQueryField, duplicateQuerySelections,
                                                                hasNoSubfields)
import           Data.Morpheus.Schema.TypeKind                 (TypeKind (..))
import           Data.Morpheus.Types.Internal.AST.RawSelection (Fragment (..), FragmentLib, RawSelection (..),
                                                                RawSelection' (..), RawSelectionSet)
import           Data.Morpheus.Types.Internal.AST.Selection    (Selection (..), SelectionRec (..), SelectionSet)
import           Data.Morpheus.Types.Internal.Base             (EnhancedKey (..))
import           Data.Morpheus.Types.Internal.Data             (DataField (..), DataOutputObject, DataType (..),
                                                                DataTypeLib (..))
import           Data.Morpheus.Types.Internal.Validation       (Validation)
import           Data.Morpheus.Types.Types                     (Variables)
import           Data.Morpheus.Validation.Arguments            (resolveArguments, validateArguments)
import           Data.Morpheus.Validation.Spread               (castFragmentType, resolveSpread)
import           Data.Morpheus.Validation.Utils.Selection      (lookupFieldAsSelectionSet, lookupSelectionField,
                                                                lookupUnionTypes, notObject)
import           Data.Morpheus.Validation.Utils.Utils          (checkNameCollision)
import           Data.Text                                     (Text)

checkDuplicatesOn :: DataOutputObject -> SelectionSet -> Validation SelectionSet
checkDuplicatesOn DataType {typeName = name'} keys = checkNameCollision enhancedKeys (map fst keys) error' >> pure keys
  where
    error' = duplicateQuerySelections name'
    enhancedKeys = map selToKey keys
    selToKey (key', Selection {selectionPosition = position'}) = EnhancedKey key' position'

clusterUnionSelection ::
     FragmentLib -> Text -> [DataOutputObject] -> (Text, RawSelection) -> Validation ([Fragment], SelectionSet)
clusterUnionSelection fragments' type' possibleTypes' = splitFrag
  where
    packFragment fragment' = return ([fragment'], [])
    typeNames = map typeName possibleTypes'
    splitFrag :: (Text, RawSelection) -> Validation ([Fragment], SelectionSet)
    splitFrag (_, Spread reference') = resolveSpread fragments' typeNames reference' >>= packFragment
    splitFrag ("__typename", RawSelectionField RawSelection' {rawSelectionPosition = position'}) =
      return
        ( []
        , [ ( "__typename"
            , Selection {selectionRec = SelectionField, selectionArguments = [], selectionPosition = position'})
          ])
    splitFrag (key', RawSelectionSet RawSelection' {rawSelectionPosition = position'}) =
      Left $ cannotQueryField key' type' position'
    splitFrag (key', RawSelectionField RawSelection' {rawSelectionPosition = position'}) =
      Left $ cannotQueryField key' type' position'
    splitFrag (key', RawAlias {rawAliasPosition = position'}) = Left $ cannotQueryField key' type' position'
    splitFrag (_, InlineFragment fragment') =
      castFragmentType Nothing (fragmentPosition fragment') typeNames fragment' >>= packFragment

categorizeTypes :: [DataOutputObject] -> [Fragment] -> [(DataOutputObject, [Fragment])]
categorizeTypes types' fragments' = map categorizeType types'
  where
    categorizeType :: DataOutputObject -> (DataOutputObject, [Fragment])
    categorizeType type' = (type', filter matches fragments')
      where
        matches fragment' = fragmentType fragment' == typeName type'

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
     DataTypeLib -> FragmentLib -> Variables -> DataOutputObject -> RawSelectionSet -> Validation SelectionSet
validateSelectionSet lib' fragments' variables' = __validate
  where
    __validate dataType'@DataType {typeName = typeName'} selectionSet' =
      concat <$> mapM validateSelection selectionSet' >>= checkDuplicatesOn dataType'
      where
        validateFragment Fragment {fragmentSelection = selection'} = __validate dataType' selection'
        {-
            get dataField and validated arguments for RawSelection
        -}
        getValidationData key' RawSelection' {rawSelectionArguments = rawArgs, rawSelectionPosition = position'} = do
          field' <- lookupSelectionField position' key' dataType'
          arguments' <- resolveArguments variables' rawArgs >>= validateArguments lib' (key', field') position'
          return (field', arguments')
        {-
             validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
        -}
        validateSelection :: (Text, RawSelection) -> Validation SelectionSet
        validateSelection (key', RawAlias {rawAliasSelection = rawSelection', rawAliasPosition = position'}) = do
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
          (dataField', arguments') <- getValidationData key' fullRawSelection'
          case fieldKind dataField' of
            UNION -> do
              (categories', __typename') <- clusterTypes
              mapM (validateCluster __typename') categories' >>= returnSelection arguments' . UnionSelection
              where clusterTypes = do
                      unionTypes' <- lookupUnionTypes position' key' lib' dataField'
                      (spreads', __typename') <-
                        flatTuple <$> mapM (clusterUnionSelection fragments' typeName' unionTypes') rawSelectors
                      return (categorizeTypes unionTypes' spreads', __typename')
                    {--
                        second arguments will be added to every selection cluster
                    -}
                    validateCluster :: SelectionSet -> (DataOutputObject, [Fragment]) -> Validation (Text, SelectionSet)
                    validateCluster sysSelection' (type', frags') = do
                      selection' <- __validate type' (concatMap fragmentSelection frags')
                      return (typeName type', sysSelection' ++ selection')
            OBJECT -> do
              fieldType' <- lookupFieldAsSelectionSet position' key' lib' dataField'
              __validate fieldType' rawSelectors >>= returnSelection arguments' . SelectionSet
            _ -> Left $ hasNoSubfields key' (fieldType dataField') position'
          where
            returnSelection arguments' selection' =
              pure
                [ ( key'
                  , Selection
                      {selectionArguments = arguments', selectionRec = selection', selectionPosition = position'})
                ]
        validateSelection (key', RawSelectionField fullRawSelection'@RawSelection' {rawSelectionPosition = position'}) = do
          (dataField', arguments') <- getValidationData key' fullRawSelection'
          _ <- notObject (key', position') dataField'
          pure
            [ ( key'
              , Selection
                  {selectionArguments = arguments', selectionRec = SelectionField, selectionPosition = position'})
            ]
        validateSelection (_, Spread reference') = resolveSpread fragments' [typeName'] reference' >>= validateFragment
        validateSelection (_, InlineFragment fragment') =
          castFragmentType Nothing (fragmentPosition fragment') [typeName'] fragment' >>= validateFragment
