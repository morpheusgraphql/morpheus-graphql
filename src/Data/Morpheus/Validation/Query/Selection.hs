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
  ( validateOperation
  )
where


-- MORPHEUS
import           Data.Morpheus.Error.Selection  ( hasNoSubfields
                                                , subfieldsNotSelected
                                                , unknownSelectionField
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( VariableDefinitions
                                                , Selection(..)
                                                , SelectionContent(..)
                                                , Fragment(..)
                                                , SelectionSet
                                                , FieldDefinition(..)
                                                , FieldsDefinition(..)
                                                , TypeContent(..)
                                                , TypeDefinition(..)
                                                , TypeRef(..)
                                                , Operation(..)
                                                , Ref(..)
                                                , Name
                                                , RAW
                                                , VALID
                                                , Arguments
                                                , Position
                                                , isEntNode
                                                )
import           Data.Morpheus.Types.Internal.AST.MergeSet
                                                ( concatTraverse )
import           Data.Morpheus.Types.Internal.Operation
                                                ( empty
                                                , singleton
                                                , Failure(..)
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation
                                                , askFieldType
                                                , constraint
                                                , Constraint(..)
                                                , selectKnown
                                                , updateScope
                                                )
import           Data.Morpheus.Validation.Query.UnionSelection
                                                (validateUnionSelection)
import           Data.Morpheus.Validation.Query.Arguments
                                                ( validateArguments )
import           Data.Morpheus.Validation.Query.Fragment
                                                ( castFragmentType
                                                , resolveSpread
                                                )

type TypeDef = (Name, FieldsDefinition)

validateOperation
  :: VariableDefinitions VALID
  -> TypeDef
  -> Operation RAW
  -> Validation (SelectionSet VALID)
validateOperation variables tyDef Operation { operationPosition, operationSelection } = do
  updateScope (fst tyDef) operationPosition
  __validate tyDef operationSelection
 where
  __validate
    :: TypeDef -> SelectionSet RAW -> Validation (SelectionSet VALID)
  __validate dataType@(typeName,fieldsDef) = concatTraverse validateSelection 
   where
    commonValidation :: Name -> Arguments RAW -> Position -> Validation (FieldDefinition, TypeContent, Arguments VALID)
    commonValidation fieldName selectionArguments selectionPosition = do
      (fieldDef :: FieldDefinition) <- selectKnown (Ref fieldName selectionPosition) fieldsDef
      (typeCont :: TypeContent) <- typeContent <$> askFieldType fieldDef
      -- validate field Argument -----
      arguments <- validateArguments
                    variables
                    fieldDef
                    selectionPosition
                    selectionArguments
      -- check field Type existence  -----
      pure (fieldDef, typeCont, arguments)
    -- validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
    validateSelection :: Selection RAW -> Validation (SelectionSet VALID)
    validateSelection sel@Selection { selectionName, selectionArguments = selArgs , selectionContent, selectionPosition } 
      = validateSelectionContent selectionContent
      where
        validateSelectionContent :: SelectionContent RAW -> Validation (SelectionSet VALID)
        validateSelectionContent SelectionField = singleton <$> selectField
         where
          selectField :: Validation (Selection VALID)
          selectField 
            | null selArgs && selectionName == "__typename" 
              = pure $ sel { selectionArguments = empty, selectionContent = SelectionField }
            | otherwise = do
              (dataField, datatypeContent, selectionArguments) <- commonValidation selectionName selArgs selectionPosition
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
            (dataField, datatype, selectionArguments) <- commonValidation selectionName selArgs selectionPosition
            selContent <- validateByTypeContent dataField datatype
            pure $ singleton $ sel { selectionArguments, selectionContent = selContent }
           where
            selectionRef :: Ref
            selectionRef = Ref selectionName selectionPosition
            validateByTypeContent :: FieldDefinition -> TypeContent -> Validation (SelectionContent VALID)
            -- Validate UnionSelection  
            validateByTypeContent dataField DataUnion {} 
              = validateUnionSelection  
                    __validate
                    selectionRef
                    rawSelection 
                    (typeName,dataField)
            -- Validate Regular selection set
            validateByTypeContent dataField DataObject {} = do
                fieldTypeDef <- askFieldType dataField 
                              >>= constraint OBJECT (selectionRef, typeConName $ fieldType dataField)
                SelectionSet <$> __validate fieldTypeDef rawSelection
            validateByTypeContent dataField _ = failure $ hasNoSubfields 
                selectionRef 
                (typeConName $fieldType dataField)
    validateSelection (Spread ref) =
      resolveSpread [typeName] ref >>= validateFragment
    validateSelection (InlineFragment fragment') =
      castFragmentType Nothing (fragmentPosition fragment') [typeName] fragment'
        >>= validateFragment
    --------------------------------------------------------------------------------
    validateFragment Fragment { fragmentSelection } = __validate dataType fragmentSelection
