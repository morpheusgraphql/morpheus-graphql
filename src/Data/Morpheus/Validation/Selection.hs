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
import           Data.Morpheus.Validation.Utils.Selection      (lookupFieldAsSelectionSet, lookupPossibleTypeKeys,
                                                                lookupPossibleTypes, lookupSelectionField, notObject)
import           Data.Morpheus.Validation.Utils.Utils          (checkNameCollision)
import           Data.Text                                     (Text)

checkDuplicatesOn :: DataOutputObject -> SelectionSet -> Validation SelectionSet
checkDuplicatesOn DataType {typeName = name'} keys = checkNameCollision enhancedKeys (map fst keys) error' >> pure keys
  where
    error' = duplicateQuerySelections name'
    enhancedKeys = map selToKey keys
    selToKey :: (Text, Selection) -> EnhancedKey
    selToKey (key', Selection {selectionPosition = position'}) = EnhancedKey key' position'

validateSelectionSet ::
     DataTypeLib -> FragmentLib -> Variables -> DataOutputObject -> RawSelectionSet -> Validation SelectionSet
validateSelectionSet lib' fragments' variables' type' selection' =
  concat <$> mapM (validateSelection lib' fragments' variables' type') selection' >>= checkDuplicatesOn type'

clusterUnionType :: FragmentLib -> Text -> [Text] -> (Text, RawSelection) -> Validation ([Fragment], SelectionSet)
clusterUnionType fragments' type' posTypes' = splitFrag
  where
    packFragment fragment' = return ([fragment'], [])
    splitFrag :: (Text, RawSelection) -> Validation ([Fragment], SelectionSet)
    splitFrag (_, Spread reference') = resolveSpread fragments' posTypes' reference' >>= packFragment
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
    splitFrag (key', Alias {aliasPosition = position'}) = Left $ cannotQueryField key' type' position'
    splitFrag (_, InlineFragment fragment') =
      castFragmentType Nothing (fragmentPosition fragment') posTypes' fragment' >>= packFragment

categorizeTypes :: [DataOutputObject] -> [Fragment] -> [(DataOutputObject, [Fragment])]
categorizeTypes types' fragments' = map categorizeType types'
  where
    categorizeType :: DataOutputObject -> (DataOutputObject, [Fragment])
    categorizeType type' = (type', filter matches fragments')
      where
        matches fragment' = fragmentType fragment' == typeName type'

flatTuple :: [([a], [b])] -> ([a], [b])
flatTuple list' = (concatMap fst list', concatMap snd list')

validateSelection ::
     DataTypeLib -> FragmentLib -> Variables -> DataOutputObject -> (Text, RawSelection) -> Validation SelectionSet
validateSelection lib' fragments' variables' parent' = validate
  where
    castFragment :: DataOutputObject -> Fragment -> Validation SelectionSet
    castFragment dataType' Fragment {fragmentSelection = selection'} =
      validateSelectionSet lib' fragments' variables' dataType' selection'
    {-
         actual validation
    -}
    validate :: (Text, RawSelection) -> Validation SelectionSet
    validate (key', RawSelectionSet RawSelection' { rawSelectionArguments = rawArgs
                                                  , rawSelectionRec = rawSelectors
                                                  , rawSelectionPosition = position'
                                                  }) = do
      field' <- lookupSelectionField position' key' parent'
      resolvedArgs' <- resolveArguments variables' rawArgs
      arguments' <- validateArguments lib' (key', field') position' resolvedArgs'
      case fieldKind field' of
        UNION -> do
          keys' <- lookupPossibleTypeKeys position' key' lib' field'
          (spreads', __typename') <-
            flatTuple <$> mapM (clusterUnionType fragments' (typeName parent') keys') rawSelectors
          possibleFieldTypes' <- lookupPossibleTypes position' key' lib' keys'
          let zippedSpreads' = categorizeTypes possibleFieldTypes' spreads'
          unionSelections' <- mapM (validateCategory __typename') zippedSpreads'
          pure
            [ ( key'
              , Selection
                  { selectionArguments = arguments'
                  , selectionRec = UnionSelection unionSelections'
                  , selectionPosition = position'
                  })
            ]
          where validateCategory :: SelectionSet -> (DataOutputObject, [Fragment]) -> Validation (Text, SelectionSet)
                validateCategory sysSelection' (type', frags') = do
                  selection' <-
                    validateSelectionSet lib' fragments' variables' type' (concatMap fragmentSelection frags')
                  return (typeName type', sysSelection' ++ selection')
        OBJECT -> do
          fieldType' <- lookupFieldAsSelectionSet position' key' lib' field'
          selections' <- validateSelectionSet lib' fragments' variables' fieldType' rawSelectors
          pure
            [ ( key'
              , Selection
                  { selectionArguments = arguments'
                  , selectionRec = SelectionSet selections'
                  , selectionPosition = position'
                  })
            ]
        _ -> Left $ hasNoSubfields key' (fieldType field') position'
    validate (key', RawSelectionField RawSelection' {rawSelectionArguments = rawArgs, rawSelectionPosition = position'}) = do
      field' <- lookupSelectionField position' key' parent' >>= notObject (key', position')
      args' <- resolveArguments variables' rawArgs
      arguments' <- validateArguments lib' (key', field') position' args'
      pure
        [ ( key'
          , Selection {selectionArguments = arguments', selectionRec = SelectionField, selectionPosition = position'})
        ]
    validate (_, Spread reference') = resolveSpread fragments' [typeName parent'] reference' >>= castFragment parent'
    validate (_, InlineFragment fragment') = validateType >>= castFragment parent'
      where
        validateType = castFragmentType Nothing (fragmentPosition fragment') [typeName parent'] fragment'
