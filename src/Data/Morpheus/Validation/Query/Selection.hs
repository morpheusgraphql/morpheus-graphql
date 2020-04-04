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


-- MORPHEUS
import           Data.Morpheus.Error.Selection  ( hasNoSubfields
                                                , subfieldsNotSelected
                                                , cannotQueryField
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
                                                , selectBy
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation
                                                , askSchema
                                                , selectKnown
                                                , lookupFieldAsSelectionSet
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

validateSelectionSet
  :: VariableDefinitions VALID
  -> TypeDef
  -> SelectionSet RAW
  -> Validation (SelectionSet VALID)
validateSelectionSet variables = __validate
 where
  __validate
    :: TypeDef -> SelectionSet RAW -> Validation (SelectionSet VALID)
  __validate dataType@(typeName,fieldsDef) = concatTraverse validateSelection 
   where
    commonValidation :: Name -> Arguments RAW -> Position -> Validation (FieldDefinition, TypeContent, Arguments VALID)
    commonValidation fieldName selectionArguments selectionPosition = do
      (fieldDef :: FieldDefinition) <- selectBy err fieldName fieldsDef
      let feildTypeName = typeConName (fieldType fieldDef)
      let fieldTypeRef = Ref feildTypeName selectionPosition
      schema <- askSchema
      -- validate field Argument -----
      arguments <- validateArguments
                    variables
                    fieldDef
                    selectionPosition
                    selectionArguments
      -- check field Type existence  -----
      (typeCont :: TypeContent) <- typeContent <$> selectKnown fieldTypeRef schema
      pure (fieldDef, typeCont, arguments)
    -- validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
        where err = cannotQueryField fieldName typeName selectionPosition -- TODO: use class Unknown to Generate Error
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
                fieldType' <- lookupFieldAsSelectionSet selectionRef dataField
                SelectionSet <$> __validate fieldType' rawSelection
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
