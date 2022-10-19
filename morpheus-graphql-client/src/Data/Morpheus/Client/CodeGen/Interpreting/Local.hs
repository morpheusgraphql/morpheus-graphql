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
    getNameByPath,
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
  (rootTypeName, localTypes) <- genLocalTypes [coerce operationTypeName] operationTypeName (toAny datatype) operationSelection
  pure
    ( RequestTypeClass
        RequestTypeDefinition
          { requestArgs = requestArgs,
            requestName = typename rootTypeName,
            requestType = operationType,
            requestQuery = T.unpack query
          }
        : localTypes <> argTypes
    )

-------------------------------------------------------------------------
-- generates selection Object Types
genLocalTypes ::
  [FieldName] ->
  TypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  LocalM (CodeGenTypeName, [ClientPreDeclaration])
genLocalTypes namespace tName = genObjectType namespace (getNameByPath namespace tName)

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
  snd <$> genLocalTypes path (getFullName $ typeFrom [] dType) dType selectionSet
subTypesBySelection namespace dType Selection {selectionPosition, selectionContent = UnionSelection interface unionSelections} =
  do
    let variants = toList unionSelections
    traverse_ (checkTypename selectionPosition namespace interface) variants
    let cgTypeName = typeFrom namespace dType
    (cons, subTypes) <- unzip <$> traverse (getVariant namespace) variants
    (fallbackCons, fallBackTypes) <- maybe (getEmptyFallback cgTypeName) (getVariant namespace . UnionTag (typeName dType)) interface
    let typeDef = CodeGenType {cgTypeName, cgConstructors = map buildVariantConstructor (cons <> [fallbackCons]), cgDerivations = defaultDerivations}
    pure ([ClientType typeDef, FromJSONUnionClass cgTypeName (map tagConstructor cons <> [(UVar "_fallback", mapFallback fallbackCons)])] <> concat subTypes <> fallBackTypes)
  where
    tagConstructor (name, x) = (UString $ typename name, (name, fmap (const "v") x))
    mapFallback (x, y) = (x, fmap (const "v") y)

checkTypename :: Position -> [FieldName] -> Maybe (SelectionSet VALID) -> UnionTag -> LocalM ()
checkTypename pos path iFace UnionTag {..}
  | any (member "__typename") (unionTagSelection : toList iFace) = pure ()
  | otherwise =
      gqlWarning $
        withPath
          ("missing \"__typename\" for selection " <> msg unionTagName <> ". this can lead to undesired behavior at runtime!")
          (map (PropName . unpackName) path)
          `at` pos

type Variant = (CodeGenTypeName, Maybe TypeName)

getEmptyFallback :: CodeGenTypeName -> LocalM (Variant, [ClientPreDeclaration])
getEmptyFallback name = pure ((name, Nothing), [])

buildVariantConstructor :: Variant -> CodeGenConstructor
buildVariantConstructor (conName, ref) =
  CodeGenConstructor
    { constructorName = conName,
      constructorFields =
        ( \fieldType ->
            CodeGenField
              { fieldName = "_",
                fieldType,
                wrappers = [],
                fieldIsNullable = False
              }
        )
          <$> toList ref
    }

getVariant :: [FieldName] -> UnionTag -> LocalM (Variant, [ClientPreDeclaration])
getVariant path (UnionTag selectedTyName selectionVariant) = do
  conDatatype <- getType selectedTyName
  let name = CodeGenTypeName path [] selectedTyName
  (n, types) <- genObjectType path name conDatatype selectionVariant
  pure
    ( ( CodeGenTypeName (path <> ["variant"]) [] selectedTyName,
        Just (getFullName n)
      ),
      types
    )

genObjectType ::
  [FieldName] ->
  CodeGenTypeName ->
  TypeDefinition ANY VALID ->
  SelectionSet VALID ->
  LocalM (CodeGenTypeName, [ClientPreDeclaration])
genObjectType namespace cgTypeName dataType recordSelSet = do
  (con, subTypes) <- toConstructorDefinition namespace cgTypeName dataType recordSelSet
  let def = CodeGenType {cgTypeName, cgConstructors = [con], cgDerivations = defaultDerivations}
  pure (cgTypeName, [ClientType def, FromJSONObjectClass cgTypeName con] <> subTypes)

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
            trans x = (x, TypeRef {typeConName = getFullName (typeFrom path x), ..})
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
