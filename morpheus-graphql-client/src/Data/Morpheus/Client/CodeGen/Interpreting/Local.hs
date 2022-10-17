{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.CodeGen.Interpreting.Local
  ( toLocalDefinitions,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.Client.CodeGen.AST
  ( ClientDeclaration,
    ClientPreDeclaration (..),
    ClientTypeDefinition (..),
    RequestTypeDefinition (..),
  )
import Data.Morpheus.Client.CodeGen.Interpreting.Core (Converter (..), compileError, deprecationWarning, getType, toClientDeclarations, toCodeGenField, typeFrom)
import Data.Morpheus.Client.CodeGen.Interpreting.PreDeclarations
  ( mapPreDeclarations,
  )
import Data.Morpheus.CodeGen.Internal.AST (CodeGenConstructor (..), CodeGenTypeName (..), fromTypeName)
import Data.Morpheus.Core (Config (..), VALIDATION_MODE (WITHOUT_VARIABLES), validateRequest)
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
    ExecutableDocument (..),
    FieldDefinition (..),
    FieldName,
    OUT,
    Operation (..),
    RAW,
    Ref (..),
    Schema (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    UnionTag (..),
    VALID,
    Variable (..),
    VariableDefinitions,
    getOperationDataType,
    getOperationName,
    mkTypeRef,
    msg,
    toAny,
  )
import qualified Data.Text as T
import Relude hiding (empty, show)
import Prelude (show)

clientConfig :: Config
clientConfig =
  Config
    { debug = False,
      validationMode = WITHOUT_VARIABLES
    }

toLocalDefinitions :: (Text, ExecutableDocument) -> Schema VALID -> GQLResult [ClientDeclaration]
toLocalDefinitions (query, request) schema = do
  validOperation <- validateRequest clientConfig schema request
  x <- flip runReaderT (schema, operationArguments $ operation request) $ runConverter $ genLocalDeclarations query validOperation
  traverse mapPreDeclarations x

genLocalDeclarations :: Text -> Operation VALID -> Converter [ClientPreDeclaration]
genLocalDeclarations query op@Operation {operationName, operationSelection, operationType} = do
  (schema, varDefs) <- asks id
  datatype <- getOperationDataType op schema
  let argumentsType = toArgumentsType (fromTypeName $ getOperationName operationName <> "Args") varDefs
  (rootType :| localTypes) <-
    genLocalTypes
      []
      (getOperationName operationName)
      (toAny datatype)
      operationSelection
  pure
    ( RequestTypeClass
        RequestTypeDefinition
          { requestArgs = maybe "()" (typename . clientTypeName) argumentsType,
            requestName = typename (clientTypeName rootType),
            requestType = operationType,
            requestQuery = T.unpack query
          }
        : concatMap toClientDeclarations (rootType : (localTypes <> maybeToList argumentsType))
    )

-------------------------------------------------------------------------
-- generates selection Object Types
genLocalTypes ::
  [FieldName] ->
  TypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  Converter (NonEmpty ClientTypeDefinition)
genLocalTypes namespace tName dataType recordSelSet = do
  let clientTypeName = CodeGenTypeName {namespace, typeParameters = [], typename = tName}
  (con, subTypes) <- toConstructorDefinition (if null namespace then [coerce tName] else namespace) clientTypeName dataType recordSelSet
  pure $
    ClientTypeDefinition
      { clientTypeName,
        clientCons = [con],
        clientKind = KindObject Nothing
      }
      :| subTypes

toConstructorDefinition ::
  [FieldName] ->
  CodeGenTypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  Converter (CodeGenConstructor, [ClientTypeDefinition])
toConstructorDefinition path name datatype selSet = do
  (fields, subTypes) <- unzip <$> traverse genField (toList selSet)
  pure (CodeGenConstructor {constructorName = name, constructorFields = map toCodeGenField fields}, concat subTypes)
  where
    genField :: Selection VALID -> Converter (FieldDefinition ANY VALID, [ClientTypeDefinition])
    genField sel = do
      let fieldName = keyOf sel
      let fieldPath = path <> [fieldName]
      (fieldDataType, fieldType) <- getFieldType fieldPath datatype sel
      subTypes <- subTypesBySelection fieldPath fieldDataType sel
      pure
        ( FieldDefinition
            { fieldName,
              fieldType,
              fieldContent = Nothing,
              fieldDescription = Nothing,
              fieldDirectives = empty
            },
          subTypes
        )

------------------------------------------
subTypesBySelection ::
  [FieldName] ->
  TypeDefinition ANY VALID ->
  Selection VALID ->
  Converter [ClientTypeDefinition]
subTypesBySelection _ _ Selection {selectionContent = SelectionField} = pure []
subTypesBySelection path dType Selection {selectionContent = SelectionSet selectionSet} =
  toList <$> genLocalTypes path (typeFrom [] dType) dType selectionSet
subTypesBySelection namespace dType Selection {selectionContent = UnionSelection interface unionSelections} =
  do
    let variants = UnionTag (typeName dType) interface : toList unionSelections
    (clientCons, subTypes) <- unzip <$> traverse (getVariantType namespace) variants
    pure
      ( ClientTypeDefinition
          { clientTypeName = CodeGenTypeName {namespace, typeParameters = [], typename = typeFrom [] dType},
            clientCons,
            clientKind = KindUnion
          }
          : concat subTypes
      )

getVariantType :: [FieldName] -> UnionTag -> Converter (CodeGenConstructor, [ClientTypeDefinition])
getVariantType path (UnionTag selectedTyName selectionVariant) = do
  conDatatype <- getType selectedTyName
  toConstructorDefinition path (CodeGenTypeName path [] selectedTyName) conDatatype selectionVariant

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
    } = toFieldDef typeContent >>= processFieldDefinition
    where
      toFieldDef :: TypeContent TRUE ANY VALID -> Converter (FieldDefinition OUT VALID)
      toFieldDef _
        | selectionName == "__typename" =
            pure
              FieldDefinition
                { fieldName = "__typename",
                  fieldDescription = Nothing,
                  fieldType = mkTypeRef "String",
                  fieldDirectives = empty,
                  fieldContent = Nothing
                }
      toFieldDef DataObject {objectFields} = selectBy selError selectionName objectFields
      toFieldDef DataInterface {interfaceFields} = selectBy selError selectionName interfaceFields
      toFieldDef dt = throwError (compileError $ "Type should be output Object \"" <> msg (show dt))
      selError = compileError $ "can't find field " <> msg selectionName <> " on type: " <> msg (show typeContent)
      --
      processFieldDefinition
        FieldDefinition
          { fieldType = TypeRef {..},
            fieldDirectives
          } =
          checkDeprecated *> (trans <$> getType typeConName)
          where
            trans x = (x, TypeRef {typeConName = typeFrom path x, ..})
            ------------------------------------------------------------------
            checkDeprecated :: Converter ()
            checkDeprecated = deprecationWarning fieldDirectives (coerce typeName, Ref selectionName selectionPosition)

toArgumentsType ::
  CodeGenTypeName ->
  VariableDefinitions RAW ->
  Maybe ClientTypeDefinition
toArgumentsType clientTypeName variables
  | null variables = Nothing
  | otherwise =
      Just
        ClientTypeDefinition
          { clientTypeName,
            clientKind = KindInputObject,
            clientCons =
              [ CodeGenConstructor
                  { constructorName = clientTypeName,
                    constructorFields = toCodeGenField . toFieldDefinition <$> toList variables
                  }
              ]
          }

toFieldDefinition :: Variable RAW -> FieldDefinition ANY VALID
toFieldDefinition Variable {variableName, variableType} =
  FieldDefinition
    { fieldName = variableName,
      fieldContent = Nothing,
      fieldType = variableType,
      fieldDescription = Nothing,
      fieldDirectives = empty
    }
