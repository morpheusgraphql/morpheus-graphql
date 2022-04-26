{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Transform.Selection
  ( toClientDefinition,
    ClientDefinition (..),
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.Client.Internal.Types
  ( ClientConstructorDefinition (..),
    ClientDefinition (..),
    ClientTypeDefinition (..),
    Mode (..),
    TypeNameTH (..),
  )
import Data.Morpheus.Client.Transform.Core (Converter (..), compileError, deprecationWarning, getType, leafType, typeFrom)
import Data.Morpheus.Client.Transform.Inputs (renderNonOutputTypes, renderOperationArguments)
import Data.Morpheus.Internal.Ext
  ( GQLResult,
  )
import Data.Morpheus.Internal.Utils
  ( empty,
    keyOf,
    selectBy,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    FieldDefinition (..),
    FieldName,
    Operation (..),
    RAW,
    Ref (..),
    Schema (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    UnionTag (..),
    VALID,
    VariableDefinitions,
    getOperationDataType,
    getOperationName,
    mkTypeRef,
    msg,
    toAny,
  )
import Relude hiding (empty, show)
import Prelude (show)

toClientDefinition ::
  Mode ->
  Schema VALID ->
  VariableDefinitions RAW ->
  Operation VALID ->
  GQLResult ClientDefinition
toClientDefinition mode schema vars = flip runReaderT (schema, vars) . runConverter . genOperation mode

genOperation :: Mode -> Operation VALID -> Converter ClientDefinition
genOperation mode operation = do
  (clientArguments, localTypes, globalTypes) <- renderOperationTypes mode operation
  globalDefinitions <- renderNonOutputTypes globalTypes
  pure ClientDefinition {clientArguments, clientTypes = localTypes <> globalDefinitions}

renderOperationTypes ::
  Mode ->
  Operation VALID ->
  Converter
    ( Maybe ClientTypeDefinition,
      [ClientTypeDefinition],
      [TypeName]
    )
renderOperationTypes mode op@Operation {operationName, operationSelection} = do
  let localGroupName = getOperationName operationName
  datatype <- asks fst >>= getOperationDataType op
  arguments <- renderOperationArguments op
  (outputTypes, globalTypes) <-
    genRecordType
      [coerce localGroupName | mode == Local]
      localGroupName
      (toAny datatype)
      operationSelection
  pure (arguments, outputTypes, if mode == Both then globalTypes else [])

-------------------------------------------------------------------------
-- generates selection Object Types
genRecordType ::
  [FieldName] ->
  TypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  Converter ([ClientTypeDefinition], [TypeName])
genRecordType path tName dataType recordSelSet = do
  (con, subTypes, requests) <- genConsD path tName dataType recordSelSet
  pure
    ( ClientTypeDefinition
        { clientTypeName = TypeNameTH path tName,
          clientCons = [con],
          clientKind = KindObject Nothing
        } :
      subTypes,
      requests
    )

genConsD ::
  [FieldName] ->
  TypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  Converter
    ( ClientConstructorDefinition,
      [ClientTypeDefinition],
      [TypeName]
    )
genConsD path cName datatype selSet = do
  (cFields, subTypes, requests) <- unzip3 <$> traverse genField (toList selSet)
  pure (ClientConstructorDefinition {cName, cFields}, concat subTypes, concat requests)
  where
    genField ::
      Selection VALID ->
      Converter (FieldDefinition ANY VALID, [ClientTypeDefinition], [TypeName])
    genField sel =
      do
        (fieldDataType, fieldType) <-
          getFieldType
            fieldPath
            datatype
            sel
        (subTypes, requests) <- subTypesBySelection fieldPath fieldDataType sel
        pure
          ( FieldDefinition
              { fieldName,
                fieldType,
                fieldContent = Nothing,
                fieldDescription = Nothing,
                fieldDirectives = empty
              },
            subTypes,
            requests
          )
      where
        fieldPath = path <> [fieldName]
        -------------------------------
        fieldName = keyOf sel

------------------------------------------
subTypesBySelection ::
  [FieldName] ->
  TypeDefinition ANY VALID ->
  Selection VALID ->
  Converter
    ( [ClientTypeDefinition],
      [TypeName]
    )
subTypesBySelection _ dType Selection {selectionContent = SelectionField} =
  leafType dType
subTypesBySelection path dType Selection {selectionContent = SelectionSet selectionSet} =
  genRecordType path (typeFrom [] dType) dType selectionSet
subTypesBySelection path dType Selection {selectionContent = UnionSelection interface unionSelections} =
  do
    (clientCons, subTypes, requests) <-
      unzip3
        <$> traverse
          getUnionType
          ( UnionTag (typeName dType) interface : toList unionSelections
          )
    pure
      ( ClientTypeDefinition
          { clientTypeName = TypeNameTH path (typeFrom [] dType),
            clientCons,
            clientKind = KindUnion
          } :
        concat subTypes,
        concat requests
      )
  where
    getUnionType (UnionTag selectedTyName selectionVariant) = do
      conDatatype <- getType selectedTyName
      genConsD path selectedTyName conDatatype selectionVariant

getFieldType ::
  [FieldName] ->
  TypeDefinition ANY VALID ->
  Selection VALID ->
  Converter (TypeDefinition ANY VALID, TypeRef)
getFieldType
  path
  TypeDefinition {typeContent, typeName}
  Selection
    { selectionName,
      selectionPosition
    }
    | selectionName == "__typename" =
      processDeprecation
        FieldDefinition
          { fieldName = "__typename",
            fieldDescription = Nothing,
            fieldType = mkTypeRef "String",
            fieldDirectives = empty,
            fieldContent = Nothing
          }
    | otherwise = withTypeContent typeContent
    where
      withTypeContent DataObject {objectFields} =
        selectBy selError selectionName objectFields >>= processDeprecation
      withTypeContent DataInterface {interfaceFields} =
        selectBy selError selectionName interfaceFields >>= processDeprecation
      withTypeContent dt =
        throwError (compileError $ "Type should be output Object \"" <> msg (show dt))
      selError = compileError $ "can't find field " <> msg selectionName <> " on type: " <> msg (show typeContent)
      processDeprecation
        FieldDefinition
          { fieldType = alias@TypeRef {typeConName},
            fieldDirectives
          } =
          checkDeprecated *> (trans <$> getType typeConName)
          where
            trans x =
              (x, alias {typeConName = typeFrom path x})
            ------------------------------------------------------------------
            checkDeprecated :: Converter ()
            checkDeprecated =
              deprecationWarning
                fieldDirectives
                ( coerce typeName,
                  Ref {refName = selectionName, refPosition = selectionPosition}
                )
