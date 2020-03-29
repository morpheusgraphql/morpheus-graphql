{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Morpheus.Validation.Query.Selection
  ( validateSelectionSet
  )
where


import           Control.Monad                  ((>=>))
import           Data.Text                      ( Text )

-- MORPHEUS
import           Data.Morpheus.Error.Selection  ( cannotQueryField
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
                                                , Fragments
                                                , RawSelection
                                                , RawSelectionSet
                                                , FieldDefinition(..)
                                                , FieldsDefinition(..)
                                                , TypeContent(..)
                                                , TypeDefinition(..)
                                                , Schema(..)
                                                , TypeRef(..)
                                                , Name
                                                , RAW
                                                , VALID
                                                , Arguments
                                                , Position
                                                , SelectionSet
                                                , isEntNode
                                                , lookupFieldAsSelectionSet
                                                , lookupSelectionField
                                                , lookupUnionTypes
                                                , UnionTag(..)
                                                )
import           Data.Morpheus.Types.Internal.AST.SelectionMap
                                                ( concatTraverse 
                                                , join
                                                )
import qualified Data.Morpheus.Types.Internal.AST.SelectionMap as SMap
                                                ( join )
import           Data.Morpheus.Types.Internal.Operation
                                                ( selectBy 
                                                , empty
                                                , singleton
                                                , Listable(..)
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


-- returns all Fragments used in Union
exploreUnionFragments
  :: Fragments
  -> Name
  -> [Name]
  -> RawSelection
  -> Validation [Fragment]
exploreUnionFragments fragments unionTypeName unionTags = splitFrag
 where
  packFragment fragment = [fragment]
  splitFrag
    :: RawSelection -> Validation [Fragment]
  splitFrag (Spread ref) = packFragment <$> resolveSpread fragments unionTags ref 
  splitFrag Selection { selectionName = "__typename",selectionContent = SelectionField } = pure []
  splitFrag Selection { selectionName, selectionPosition } =
    failure $ cannotQueryField selectionName unionTypeName selectionPosition
  splitFrag (InlineFragment fragment) = packFragment <$>
    castFragmentType Nothing (fragmentPosition fragment) unionTags fragment

-- sorts Fragment by contitional Types
-- [
--   ( Type for Tag User , [ Fragment for User] )
--   ( Type for Tag Product , [ Fragment for Product] )
-- ]
tagUnionFragments
  :: [TypeDef] -> [Fragment] -> [(TypeDef, [Fragment])]
tagUnionFragments types fragments = filter notEmpty $ map categorizeType types
 where
  notEmpty = not . null . snd
  categorizeType :: (Name, FieldsDefinition) -> (TypeDef, [Fragment])
  categorizeType datatype = (datatype, filter matches fragments)
    where matches fragment = fragmentType fragment == fst datatype


type TypeDef = (Name, FieldsDefinition)
type TypeFieldDef = (Name, FieldDefinition)
type SelectionDef s = (Name,Position,Arguments s)


clusterTypes :: Schema -> Fragments -> SelectionDef RAW -> SelectionSet RAW -> TypeFieldDef -> Validation [(TypeDef, [Fragment])]
clusterTypes schema fragments (selectionName,selectionPosition,_) selectionSet (typeName,dataField) = do
  -- get union Types defined in GraphQL schema -> (union Tag, union Selection set)
  -- for example 
  -- User | Admin | Product
  unionTypes <- lookupUnionTypes selectionPosition
                                selectionName
                                schema
                                dataField
  let unionTags = map fst unionTypes
  -- find all Fragments used in Selection
  spreads <- concat <$> traverse (exploreUnionFragments fragments typeName unionTags) (toList selectionSet)
  -- 
  pure $ tagUnionFragments unionTypes spreads


{-
    - all Variable and Fragment references will be: resolved and validated
    - unionTypes: will be clustered under type names
      ...A on T1 {<SelectionA>}
      ...B on T2 {<SelectionB>}
      ...C on T2 {<SelectionC>}
      will be become : [
          UnionTag "T1" {<SelectionA>},
          UnionTag "T2" {<SelectionB>,<SelectionC>}
      ]
 -}
validateCluster
      :: (TypeDef -> RawSelectionSet -> Validation ValidSelectionSet)
      -> [(TypeDef, [Fragment])]
      -> Validation (SelectionContent VALID)
validateCluster validator = traverse _validateCluster >=> fmap UnionSelection . fromList
 where
  _validateCluster :: (TypeDef, [Fragment]) -> Validation UnionTag
  _validateCluster  (unionType, fragmets) = do
        fragmentSelections <- SMap.join $ map fragmentSelection fragmets
        selection <- validator unionType fragmentSelections
        pure $ UnionTag (fst unionType) selection
    
validateSelectionSet
  :: Schema
  -> Fragments
  -> Text
  -> ValidVariables
  -> TypeDef
  -> RawSelectionSet
  -> Validation ValidSelectionSet
validateSelectionSet lib fragments operatorName variables = __validate
 where
  __validate
    :: TypeDef -> RawSelectionSet -> Validation ValidSelectionSet
  __validate dataType@(typeName,_) = concatTraverse validateSelection 
   where
    -- getValidationData :: Name -> Arguments RAW -> Position -> (FieldDefinition, TypeContent, Arguments VALID)
    getValidationData key selectionArguments selectionPosition = do
      selectionField <- lookupSelectionField selectionPosition key dataType
      -- validate field Argument -----
      arguments <- validateArguments lib
                                     operatorName
                                     variables
                                     selectionField
                                     selectionPosition
                                     selectionArguments
      -- check field Type existence  -----
      fieldDataType <- selectBy
        (unknownType (typeConName $fieldType selectionField) selectionPosition) 
        (typeConName $ fieldType selectionField)
        lib
      pure (selectionField, typeContent fieldDataType, arguments)
    -- validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
    --
    validateSelection :: RawSelection -> Validation ValidSelectionSet
    validateSelection sel@Selection { selectionName, selectionArguments = selArgs , selectionContent, selectionPosition } 
      = validateSelectionContent selectionContent
      where
        validateSelectionContent :: SelectionContent RAW -> Validation ValidSelectionSet
        validateSelectionContent SelectionField = singleton <$> selectField
         where
          selectField :: Validation ValidSelection
          selectField 
            | null selArgs && selectionName == "__typename" 
              = pure $ sel { selectionArguments = empty, selectionContent = SelectionField }
            | otherwise = do
              (dataField, datatypeContent, selectionArguments) <- getValidationData selectionName selArgs selectionPosition
              isLeaf datatypeContent dataField
              pure $ sel { selectionArguments, selectionContent = SelectionField }
          ------------------------------------------------------------
          isLeaf :: TypeContent -> FieldDefinition -> Validation ()
          isLeaf datatype FieldDefinition { fieldType = TypeRef { typeConName } }
              | isEntNode datatype = pure ()
              | otherwise = failure
              $ subfieldsNotSelected selectionName typeConName selectionPosition
        ----- SelectionSet
        validateSelectionContent (SelectionSet rawSelection)
          = do
            (dataField, datatype, selectionArguments) <- getValidationData selectionName selArgs selectionPosition
            selContent <- validateByTypeContent dataField datatype
            pure $ singleton $ sel { selectionArguments, selectionContent = selContent }
           where
            validateByTypeContent :: FieldDefinition -> TypeContent -> Validation (SelectionContent VALID)
            -- Validate UnionSelection  
            validateByTypeContent dataField DataUnion {} = do
                categories <- clusterTypes 
                    lib 
                    fragments
                    (selectionName,selectionPosition,selArgs) 
                    rawSelection 
                    (typeName,dataField)
                validateCluster __validate categories
            -- Validate Regular selection set
            validateByTypeContent dataField DataObject {} = do
                fieldType' <- lookupFieldAsSelectionSet selectionPosition
                                                        selectionName
                                                        lib
                                                        dataField
                SelectionSet <$> __validate fieldType' rawSelection

            validateByTypeContent dataField _ = failure $ hasNoSubfields selectionName
                                            (typeConName $fieldType dataField)
                                            selectionPosition
    validateSelection (Spread ref) =
      resolveSpread fragments [typeName] ref >>= validateFragment
    validateSelection (InlineFragment fragment') =
      castFragmentType Nothing (fragmentPosition fragment') [typeName] fragment'
        >>= validateFragment
    --------------------------------------------------------------------------------
    validateFragment Fragment { fragmentSelection } = __validate dataType fragmentSelection
