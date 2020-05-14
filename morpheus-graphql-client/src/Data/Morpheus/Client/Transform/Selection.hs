{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Morpheus.Client.Transform.Selection
  ( toClientDefinition,
    ClientDefinition (..),
  )
where

--
-- MORPHEUS
import Control.Monad.Reader (asks, runReaderT)
import Data.Morpheus.Client.Transform.Core (Converter (..), compileError, deprecationWarning, getType, leafType, typeFrom)
import Data.Morpheus.Client.Transform.Inputs (renderNonOutputTypes, renderOperationArguments)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentsDefinition (..),
    ConsD (..),
    DataTypeKind (..),
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
    TypeD (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    UnionTag (..),
    VALID,
    VariableDefinitions,
    getOperationDataType,
    getOperationName,
    msg,
    toAny,
    toFieldName,
  )
import Data.Morpheus.Types.Internal.Operation
  ( Failure (..),
    Listable (..),
    keyOf,
    selectBy,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
  )
import Data.Semigroup ((<>))

data ClientDefinition = ClientDefinition
  { clientArguments :: Maybe TypeD,
    clientTypes :: [TypeD]
  }
  deriving (Show)

toClientDefinition ::
  Schema ->
  VariableDefinitions RAW ->
  Operation VALID ->
  Eventless ClientDefinition
toClientDefinition schema vars = flip runReaderT (schema, vars) . runConverter . genOperation

genOperation :: Operation VALID -> Converter ClientDefinition
genOperation operation = do
  (clientArguments, outputTypes, enums) <- renderOperationType operation
  nonOutputTypes <- renderNonOutputTypes enums
  pure ClientDefinition {clientArguments, clientTypes = outputTypes <> nonOutputTypes}

renderOperationType :: Operation VALID -> Converter (Maybe TypeD, [TypeD], [TypeName])
renderOperationType op@Operation {operationName, operationSelection} = do
  datatype <- asks fst >>= getOperationDataType op
  arguments <- renderOperationArguments op
  (outputTypes, enums) <-
    genRecordType
      []
      (getOperationName operationName)
      (toAny datatype)
      operationSelection
  pure (arguments, outputTypes, enums)

-------------------------------------------------------------------------
-- generates selection Object Types
genRecordType ::
  [FieldName] ->
  TypeName ->
  TypeDefinition ANY ->
  SelectionSet VALID ->
  Converter ([TypeD], [TypeName])
genRecordType path tName dataType recordSelSet = do
  (con, subTypes, requests) <- genConsD path tName dataType recordSelSet
  pure
    ( TypeD
        { tName,
          tNamespace = path,
          tCons = [con],
          tKind = KindObject Nothing,
          tMeta = Nothing
        }
        : subTypes,
      requests
    )

genConsD ::
  [FieldName] ->
  TypeName ->
  TypeDefinition ANY ->
  SelectionSet VALID ->
  Converter (ConsD, [TypeD], [TypeName])
genConsD path cName datatype selSet = do
  (cFields, subTypes, requests) <- unzip3 <$> traverse genField (toList selSet)
  pure (ConsD {cName, cFields}, concat subTypes, concat requests)
  where
    genField ::
      Selection VALID ->
      Converter (FieldDefinition, [TypeD], [TypeName])
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
                fieldArgs = NoArguments,
                fieldMeta = Nothing
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
  TypeDefinition ANY ->
  Selection VALID ->
  Converter ([TypeD], [TypeName])
subTypesBySelection _ dType Selection {selectionContent = SelectionField} =
  leafType dType
subTypesBySelection path dType Selection {selectionContent = SelectionSet selectionSet} =
  genRecordType path (typeFrom [] dType) dType selectionSet
subTypesBySelection path dType Selection {selectionContent = UnionSelection unionSelections} =
  do
    (tCons, subTypes, requests) <-
      unzip3 <$> traverse getUnionType (toList unionSelections)
    pure
      ( TypeD
          { tNamespace = path,
            tName = typeFrom [] dType,
            tCons,
            tKind = KindUnion,
            tMeta = Nothing
          }
          : concat subTypes,
        concat requests
      )
  where
    getUnionType (UnionTag selectedTyName selectionVariant) = do
      conDatatype <- getType selectedTyName
      genConsD path selectedTyName conDatatype selectionVariant

getFieldType ::
  [FieldName] ->
  TypeDefinition ANY ->
  Selection VALID ->
  Converter (TypeDefinition ANY, TypeRef)
getFieldType
  path
  TypeDefinition {typeContent = DataObject {objectFields}, typeName}
  Selection
    { selectionName,
      selectionPosition
    } =
    selectBy selError selectionName objectFields >>= processDeprecation
    where
      selError = compileError $ "cant find field " <> msg (show objectFields)
      processDeprecation FieldDefinition {fieldType = alias@TypeRef {typeConName}, fieldMeta} =
        checkDeprecated >> (trans <$> getType typeConName)
        where
          trans x =
            (x, alias {typeConName = typeFrom path x, typeArgs = Nothing})
          ------------------------------------------------------------------
          checkDeprecated :: Converter ()
          checkDeprecated =
            deprecationWarning
              fieldMeta
              (toFieldName typeName, Ref {refName = selectionName, refPosition = selectionPosition})
getFieldType _ dt _ =
  failure (compileError $ "Type should be output Object \"" <> msg (show dt))
