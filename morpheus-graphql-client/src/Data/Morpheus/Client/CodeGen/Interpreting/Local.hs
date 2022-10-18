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
    DERIVING_MODE (..),
    RequestTypeDefinition (..),
    UnionPat (..),
  )
import Data.Morpheus.Client.CodeGen.Interpreting.Core
  ( LocalContext (..),
    LocalM (..),
    compileError,
    defaultDerivations,
    deprecationWarning,
    getType,
    gqlWarning,
    runLocalM,
    typeFrom,
  )
import Data.Morpheus.Client.CodeGen.Interpreting.PreDeclarations
  ( mapPreDeclarations,
  )
import Data.Morpheus.CodeGen.Internal.AST (CodeGenConstructor (..), CodeGenField (..), CodeGenType (..), CodeGenTypeName (..), FIELD_TYPE_WRAPPER (..), fromTypeName, getFullName)
import Data.Morpheus.Core (Config (..), VALIDATION_MODE (WITHOUT_VARIABLES), validateRequest)
import Data.Morpheus.Internal.Ext
  ( GQLResult,
  )
import Data.Morpheus.Internal.Utils
  ( empty,
    keyOf,
    member,
    selectBy,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ExecutableDocument (..),
    FieldDefinition (..),
    FieldName,
    OUT,
    Operation (..),
    Position (..),
    PropName (..),
    RAW,
    Ref (..),
    Schema (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    UnionTag (..),
    VALID,
    Variable (..),
    VariableDefinitions,
    at,
    getOperationDataType,
    getOperationName,
    isNullable,
    mkTypeRef,
    msg,
    toAny,
    unpackName,
    withPath,
  )
import qualified Data.Text as T
import Relude hiding (empty, show)
import Prelude (show)

clientConfig :: Config
clientConfig = Config {debug = False, validationMode = WITHOUT_VARIABLES}

toLocalDefinitions :: (Text, ExecutableDocument) -> Schema VALID -> GQLResult [ClientDeclaration]
toLocalDefinitions (query, request) ctxSchema = do
  validOperation <- validateRequest clientConfig ctxSchema request
  let context =
        LocalContext
          { ctxSchema,
            ctxVariables = operationArguments $ operation request,
            ctxPosition = Nothing
          }
  x <- runLocalM context $ genLocalDeclarations query validOperation
  traverse mapPreDeclarations x

genLocalDeclarations :: Text -> Operation VALID -> LocalM [ClientPreDeclaration]
genLocalDeclarations query op@Operation {operationName, operationSelection, operationType} = do
  LocalContext {ctxSchema, ctxVariables} <- asks id
  datatype <- getOperationDataType op ctxSchema
  let operationTypeName = getOperationName operationName
  let (requestArgs, argTypes) = toArgumentsType operationTypeName ctxVariables
  (rootTypeName, localTypes) <- genLocalTypes [] operationTypeName (toAny datatype) operationSelection
  pure
    ( RequestTypeClass
        RequestTypeDefinition
          { requestArgs = requestArgs,
            requestName = typename rootTypeName,
            requestType = operationType,
            requestQuery = T.unpack query
          } :
      localTypes <> argTypes
    )

-------------------------------------------------------------------------
-- generates selection Object Types
genLocalTypes ::
  [FieldName] ->
  TypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  LocalM (CodeGenTypeName, [ClientPreDeclaration])
genLocalTypes namespace tName dataType recordSelSet = do
  let cgTypeName = CodeGenTypeName {namespace, typeParameters = [], typename = tName}
  (con, subTypes) <- toConstructorDefinition (if null namespace then [coerce tName] else namespace) cgTypeName dataType recordSelSet
  let def = CodeGenType {cgTypeName, cgConstructors = [con], cgDerivations = defaultDerivations}
  pure (cgTypeName, [ClientType def, FromJSONObjectClass cgTypeName con] <> subTypes)

toConstructorDefinition ::
  [FieldName] ->
  CodeGenTypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  LocalM (CodeGenConstructor, [ClientPreDeclaration])
toConstructorDefinition path name datatype selSet = do
  (fields, subTypes) <- unzip <$> traverse genField (toList selSet)
  pure (CodeGenConstructor {constructorName = name, constructorFields = fields}, concat subTypes)
  where
    genField :: Selection VALID -> LocalM (CodeGenField, [ClientPreDeclaration])
    genField sel = do
      let fieldName = keyOf sel
      let fieldPath = path <> [fieldName]
      (fieldDataType, ref@TypeRef {..}) <- getFieldType fieldPath datatype sel
      subTypes <- subTypesBySelection fieldPath fieldDataType sel
      pure
        ( CodeGenField
            { fieldName,
              fieldType = typeConName,
              wrappers = [GQL_WRAPPER typeWrappers],
              fieldIsNullable = isNullable ref
            },
          subTypes
        )

------------------------------------------
subTypesBySelection ::
  [FieldName] ->
  TypeDefinition ANY VALID ->
  Selection VALID ->
  LocalM [ClientPreDeclaration]
subTypesBySelection _ _ Selection {selectionContent = SelectionField} = pure []
subTypesBySelection path dType Selection {selectionContent = SelectionSet selectionSet} =
  snd <$> genLocalTypes path (typeFrom [] dType) dType selectionSet
subTypesBySelection namespace dType Selection {selectionPosition, selectionContent = UnionSelection interface unionSelections} =
  do
    let variants = toList unionSelections
    traverse_ (checkTypename selectionPosition namespace interface) variants
    (cons, subTypes) <- unzip <$> traverse (getVariantType namespace) variants
    (fallbackCons, fallBackTypes) <- maybe (getEmptyFallback namespace (typeName dType)) (getVariantType namespace . UnionTag (typeName dType)) interface
    let cgTypeName = CodeGenTypeName {namespace, typeParameters = [], typename = typeFrom [] dType}
    let typeDef = CodeGenType {cgTypeName, cgConstructors = cons <> [fallbackCons], cgDerivations = defaultDerivations}
    pure ([ClientType typeDef, FromJSONUnionClass cgTypeName (map tagConstructor cons <> [(UVar "_fallback", fallbackCons)])] <> concat subTypes <> fallBackTypes)
  where
    tagConstructor x = (UString $ typename $ constructorName x, x)

checkTypename :: Position -> [FieldName] -> Maybe (SelectionSet VALID) -> UnionTag -> LocalM ()
checkTypename pos path iFace UnionTag {..}
  | any (member "__typename") (unionTagSelection : toList iFace) = pure ()
  | otherwise =
    gqlWarning $
      withPath
        ("missing \"__typename\" for union selection " <> msg unionTagName <> ". this can lead to undesired behavior at runtime!")
        (map (PropName . unpackName) path)
        `at` pos

getEmptyFallback :: [FieldName] -> TypeName -> LocalM (CodeGenConstructor, [ClientPreDeclaration])
getEmptyFallback path selectedTyName =
  pure
    ( CodeGenConstructor
        { constructorName = CodeGenTypeName path [] selectedTyName,
          constructorFields = []
        },
      []
    )

getVariantType :: [FieldName] -> UnionTag -> LocalM (CodeGenConstructor, [ClientPreDeclaration])
getVariantType path (UnionTag selectedTyName selectionVariant) = do
  conDatatype <- getType selectedTyName
  toConstructorDefinition path (CodeGenTypeName path [] selectedTyName) conDatatype selectionVariant

getFieldType ::
  [FieldName] ->
  TypeDefinition ANY VALID ->
  Selection VALID ->
  LocalM (TypeDefinition ANY VALID, TypeRef)
getFieldType
  path
  TypeDefinition {typeContent, typeName}
  Selection
    { selectionName,
      selectionPosition
    } = toFieldDef typeContent >>= processFieldDefinition
    where
      toFieldDef :: TypeContent TRUE ANY VALID -> LocalM (FieldDefinition OUT VALID)
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
            checkDeprecated :: LocalM ()
            checkDeprecated = deprecationWarning fieldDirectives (coerce typeName, Ref selectionName selectionPosition)

toArgumentsType ::
  TypeName ->
  VariableDefinitions RAW ->
  (TypeName, [ClientPreDeclaration])
toArgumentsType operationTypeName variables
  | null variables = ("()", [])
  | otherwise = (getFullName cgTypeName, [ClientType def, ToJSONClass TYPE_MODE def])
  where
    def =
      CodeGenType
        { cgTypeName,
          cgConstructors = [CodeGenConstructor {constructorName = cgTypeName, constructorFields = packAsCodeGenField <$> toList variables}],
          cgDerivations = defaultDerivations
        }
    cgTypeName = fromTypeName $ operationTypeName <> "Args"

packAsCodeGenField :: Variable RAW -> CodeGenField
packAsCodeGenField Variable {variableName, variableType = ref@TypeRef {..}} =
  CodeGenField
    { fieldName = variableName,
      fieldType = typeConName,
      wrappers = [GQL_WRAPPER typeWrappers],
      fieldIsNullable = isNullable ref
    }
