{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Interpreting.Transform
  ( parseServerTypeDefinitions,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConstructor (..),
    CodeGenField (..),
    CodeGenType (..),
    CodeGenTypeName (CodeGenTypeName),
    fromTypeName,
    getFullName,
  )
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
    DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
    GQLDirectiveTypeClass (..),
    GQLTypeDefinition (..),
    InterfaceDefinition (..),
    Kind (..),
    ServerDeclaration (..),
    ServerDirectiveUsage (..),
    TypeValue (..),
  )
import Data.Morpheus.CodeGen.Server.Interpreting.Utils (CodeGenMonad (printWarnings), CodeGenT, TypeContext (..), getEnumName, getFieldName, isParamResolverType, isSubscription, lookupFieldType)
import Data.Morpheus.CodeGen.Utils
  ( camelCaseTypeName,
    toHaskellTypeName,
  )
import Data.Morpheus.Core (internalSchema, parseDefinitions, render)
import Data.Morpheus.Error (renderGQLErrors)
import Data.Morpheus.Internal.Ext (Result (..))
import Data.Morpheus.Internal.Utils (IsMap, selectOr)
import Data.Morpheus.Server.Types (Arg, SubscriptionField)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    Argument (..),
    ArgumentDefinition (..),
    CONST,
    DataEnumValue (..),
    Description,
    Directive (Directive, directiveArgs, directiveName),
    DirectiveDefinition (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    IN,
    OUT,
    ObjectEntry (..),
    RawTypeDefinition (..),
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    UnionMember (..),
    isNullable,
    isPossibleInterfaceType,
    isResolverType,
    kindOf,
    packName,
    unpackName,
  )
import Data.Morpheus.Types.Internal.AST qualified as AST
import Relude hiding (ByteString, get)

parseServerTypeDefinitions :: CodeGenMonad m => CodeGenConfig -> ByteString -> m [ServerDeclaration]
parseServerTypeDefinitions ctx txt =
  case parseDefinitions txt of
    Failure errors -> fail (renderGQLErrors errors)
    Success {result, warnings} -> printWarnings warnings >> toTHDefinitions (namespace ctx) result

toTHDefinitions ::
  CodeGenMonad m =>
  Bool ->
  [RawTypeDefinition] ->
  m [ServerDeclaration]
toTHDefinitions namespace defs = concat <$> traverse generateTypes defs
  where
    typeDefinitions = [td | RawTypeDefinition td <- defs]
    directiveDefinitions = [td | RawDirectiveDefinition td <- defs]
    generateTypes :: CodeGenMonad m => RawTypeDefinition -> m [ServerDeclaration]
    generateTypes (RawTypeDefinition typeDef) =
      runReaderT
        (genTypeDefinition typeDef)
        TypeContext
          { toArgsTypeName = mkArgsTypeName namespace (typeName typeDef),
            typeDefinitions,
            directiveDefinitions,
            currentTypeName = Just (typeName typeDef),
            currentKind = Just (kindOf typeDef),
            hasNamespace = namespace
          }
    generateTypes (RawDirectiveDefinition DirectiveDefinition {..}) =
      runReaderT
        ( do
            fields <- traverse renderDataField (argument <$> toList directiveDefinitionArgs)
            let typename = coerce directiveDefinitionName
            namespaceDirs <- getNamespaceDirs (unpackName typename)
            let cgTypeName = fromTypeName typename
            pure
              [ DataType
                  CodeGenType
                    { cgTypeName,
                      cgConstructors = [CodeGenConstructor (fromTypeName typename) fields],
                      cgDerivations = [SHOW, GENERIC]
                    },
                GQLDirectiveInstance
                  GQLDirectiveTypeClass
                    { directiveTypeName = cgTypeName,
                      directiveLocations = directiveDefinitionLocations
                    },
                GQLTypeInstance
                  GQLTypeDefinition
                    { gqlTarget = cgTypeName,
                      gqlKind = Type,
                      gqlTypeDefaultValues = mempty,
                      gqlTypeDirectiveUses = namespaceDirs
                    }
              ]
        )
        TypeContext
          { toArgsTypeName = coerce,
            typeDefinitions,
            currentTypeName = Just (coerce directiveDefinitionName),
            directiveDefinitions,
            currentKind = Nothing,
            hasNamespace = namespace
          }
    generateTypes _ = pure []

getNamespaceDirs :: MonadReader (TypeContext s) m => Text -> m [ServerDirectiveUsage]
getNamespaceDirs genTypeName = do
  namespaces <- asks hasNamespace
  pure [TypeDirectiveUsage (dirDropNamespace genTypeName) | namespaces]

inType :: MonadReader (TypeContext s) m => Maybe TypeName -> m a -> m a
inType name = local (\x -> x {currentTypeName = name, currentKind = Nothing})

mkInterfaceName :: TypeName -> TypeName
mkInterfaceName = ("Interface" <>)

mkPossibleTypesName :: TypeName -> TypeName
mkPossibleTypesName = ("PossibleTypes" <>)

genTypeDefinition ::
  CodeGenMonad m =>
  TypeDefinition ANY CONST ->
  CodeGenT m [ServerDeclaration]
genTypeDefinition
  typeDef@TypeDefinition {typeName = originalTypeName, typeContent} =
    case tKind of
      KindScalar -> do
        scalarGQLType <- deriveGQL
        pure [ScalarType (toHaskellTypeName typeName), scalarGQLType]
      _ -> genTypeContent originalTypeName typeContent >>= withType
    where
      typeName
        | tKind == KindInterface = mkInterfaceName originalTypeName
        | otherwise = originalTypeName
      tKind = kindOf typeDef
      cgTypeName = CodeGenTypeName [] ["m" | isResolverType tKind] (packName $ toHaskellTypeName typeName)
      renameDir = [TypeDirectiveUsage (dirRename (unpackName originalTypeName)) | tKind == KindInterface]
      deriveGQL = do
        namespaceDirs <- getNamespaceDirs (unpackName typeName)
        dirs <- getDirs typeDef
        -- TODO: here
        pure $
          GQLTypeInstance $
            GQLTypeDefinition
              { gqlTarget = cgTypeName,
                gqlTypeDirectiveUses = renameDir <> namespaceDirs <> dirs,
                gqlKind = derivingKind tKind,
                gqlTypeDefaultValues =
                  fromList $
                    mapMaybe getDefaultValue $
                      getInputFields typeDef
              }
      cgDerivations = derivesClasses (isResolverType tKind)
      -------------------------
      withType (ConsIN cgConstructors) = do
        gqlType <- deriveGQL
        pure [DataType CodeGenType {..}, gqlType]
      withType (ConsOUT others cgConstructors) = do
        gqlType <- deriveGQL
        pure (DataType CodeGenType {..} : gqlType : others)

derivingKind :: TypeKind -> Kind
derivingKind KindScalar = Scalar
derivingKind _ = Type

derivesClasses :: Bool -> [DerivingClass]
derivesClasses isResolver = GENERIC : [SHOW | not isResolver]

mkObjectCons :: TypeName -> [CodeGenField] -> [CodeGenConstructor]
mkObjectCons name = pure . CodeGenConstructor (fromTypeName name)

mkArgsTypeName :: Bool -> TypeName -> FieldName -> TypeName
mkArgsTypeName namespace typeName fieldName
  | namespace = typeName <> argTName
  | otherwise = argTName
  where
    argTName = camelCaseTypeName [fieldName] "Args"

mkObjectField ::
  CodeGenMonad m =>
  FieldDefinition OUT CONST ->
  CodeGenT m CodeGenField
mkObjectField
  FieldDefinition
    { fieldName = fName,
      fieldContent,
      fieldType = TypeRef {typeConName, typeWrappers}
    } = do
    isParametrized <- isParamResolverType typeConName
    genName <- asks toArgsTypeName
    kind <- asks currentKind
    fieldName <- getFieldName fName
    pure
      CodeGenField
        { fieldType = packName (toHaskellTypeName typeConName),
          fieldIsNullable = isNullable typeWrappers,
          wrappers =
            mkFieldArguments fName genName (toArgList fieldContent)
              <> [SUBSCRIPTION ''SubscriptionField | fmap isSubscription kind == Just True]
              <> [MONAD]
              <> [GQL_WRAPPER typeWrappers]
              <> [PARAMETRIZED | isParametrized],
          ..
        }

mkFieldArguments :: FieldName -> (FieldName -> TypeName) -> [ArgumentDefinition s] -> [FIELD_TYPE_WRAPPER]
mkFieldArguments _ _ [] = []
mkFieldArguments
  _
  _
  [ ArgumentDefinition FieldDefinition {fieldName, fieldType}
    ] = [TAGGED_ARG ''Arg fieldName fieldType]
mkFieldArguments fName genName _ = [ARG (genName fName)]

toArgList :: Maybe (FieldContent bool cat s) -> [ArgumentDefinition s]
toArgList (Just (FieldArgs args)) = toList args
toArgList _ = []

data BuildPlan
  = ConsIN [CodeGenConstructor]
  | ConsOUT [ServerDeclaration] [CodeGenConstructor]

genInterfaceUnion :: Monad m => TypeName -> CodeGenT m [ServerDeclaration]
genInterfaceUnion interfaceName =
  mkInterface . map typeName . mapMaybe (isPossibleInterfaceType interfaceName)
    <$> asks typeDefinitions
  where
    mkInterface [] = []
    mkInterface [possibleTypeName] = [mkGuardWithPossibleType possibleTypeName]
    mkInterface members =
      [ mkGuardWithPossibleType tName,
        DataType
          CodeGenType
            { cgTypeName = possTypeName,
              cgConstructors = map (mkUnionFieldDefinition tName) members,
              cgDerivations = derivesClasses True
            },
        GQLTypeInstance
          GQLTypeDefinition
            { gqlTarget = possTypeName,
              gqlKind = Type,
              gqlTypeDirectiveUses = empty,
              gqlTypeDefaultValues = mempty
            }
      ]
      where
        possTypeName = CodeGenTypeName [] ["m"] (packName $ toHaskellTypeName tName)
    mkGuardWithPossibleType = InterfaceType . InterfaceDefinition interfaceName (mkInterfaceName interfaceName)
    tName = mkPossibleTypesName interfaceName

mkConsEnum :: Monad m => DataEnumValue CONST -> CodeGenT m CodeGenConstructor
mkConsEnum DataEnumValue {enumName} = do
  constructorName <- getEnumName enumName
  pure CodeGenConstructor {constructorName, constructorFields = []}

renderDataField :: Monad m => FieldDefinition c CONST -> CodeGenT m CodeGenField
renderDataField FieldDefinition {fieldType = TypeRef {typeConName, typeWrappers}, fieldName = fName} = do
  fieldName <- getFieldName fName
  let wrappers = [GQL_WRAPPER typeWrappers]
  let fieldType = packName (toHaskellTypeName typeConName)
  let fieldIsNullable = isNullable typeWrappers
  pure CodeGenField {..}

genTypeContent ::
  CodeGenMonad m =>
  TypeName ->
  TypeContent TRUE ANY CONST ->
  CodeGenT m BuildPlan
genTypeContent _ DataScalar {} = pure (ConsIN [])
genTypeContent _ (DataEnum tags) = ConsIN <$> traverse mkConsEnum tags
genTypeContent typeName (DataInputObject fields) =
  ConsIN . mkObjectCons typeName <$> traverse renderDataField (toList fields)
genTypeContent _ DataInputUnion {} = fail "Input Unions not Supported"
genTypeContent typeName DataInterface {interfaceFields} =
  ConsOUT
    <$> ((<>) <$> genArgumentTypes interfaceFields <*> genInterfaceUnion typeName)
    <*> ( do
            let interfaceName = mkInterfaceName typeName
            inType
              (Just interfaceName)
              ( mkObjectCons interfaceName
                  <$> traverse mkObjectField (toList interfaceFields)
              )
        )
genTypeContent typeName DataObject {objectFields} =
  ConsOUT
    <$> genArgumentTypes objectFields
    <*> ( mkObjectCons typeName
            <$> traverse mkObjectField (toList objectFields)
        )
genTypeContent typeName (DataUnion members) =
  pure $ ConsOUT [] (unionCon <$> toList members)
  where
    unionCon UnionMember {memberName} = mkUnionFieldDefinition typeName memberName

mkUnionFieldDefinition :: TypeName -> TypeName -> CodeGenConstructor
mkUnionFieldDefinition typeName memberName =
  CodeGenConstructor
    { constructorName,
      constructorFields =
        [ CodeGenField
            { fieldName = coerce ("un" <> getFullName constructorName),
              fieldType = packName (toHaskellTypeName memberName),
              wrappers = [PARAMETRIZED],
              fieldIsNullable = False
            }
        ]
    }
  where
    constructorName = CodeGenTypeName [coerce typeName] [] memberName

genArgumentTypes :: MonadFail m => FieldsDefinition OUT CONST -> CodeGenT m [ServerDeclaration]
genArgumentTypes = fmap concat . traverse genArgumentType . toList

genArgumentType :: MonadFail m => FieldDefinition OUT CONST -> CodeGenT m [ServerDeclaration]
genArgumentType
  FieldDefinition
    { fieldName,
      fieldContent = Just (FieldArgs arguments)
    }
    | length arguments > 1 = do
        tName <- (fieldName &) <$> asks toArgsTypeName
        inType (Just tName) $ do
          let argumentFields = argument <$> toList arguments
          fields <- traverse renderDataField argumentFields
          let typename = toHaskellTypeName tName
          namespaceDirs <- getNamespaceDirs typename
          dirs <- concat <$> traverse getDirs argumentFields
          let cgTypeName = fromTypeName (packName typename)
          pure
            [ DataType
                CodeGenType
                  { cgTypeName,
                    cgConstructors = mkObjectCons tName fields,
                    cgDerivations = derivesClasses False
                  },
              GQLTypeInstance
                GQLTypeDefinition
                  { gqlTarget = cgTypeName,
                    gqlKind = Type,
                    gqlTypeDefaultValues = fromList (mapMaybe getDefaultValue argumentFields),
                    gqlTypeDirectiveUses = namespaceDirs <> dirs
                  }
            ]
genArgumentType _ = pure []

class Meta a where
  getDirs :: MonadFail m => a -> CodeGenT m [ServerDirectiveUsage]

instance (Meta a) => Meta (Maybe a) where
  getDirs (Just x) = getDirs x
  getDirs _ = pure []

descDirective :: Maybe Description -> [TypeValue]
descDirective desc = map describe (maybeToList desc)
  where
    describe x = TypeValueObject "Describe" [("text", TypeValueString x)]

dirDropNamespace :: Text -> TypeValue
dirDropNamespace name = TypeValueObject "DropNamespace" [("dropNamespace", TypeValueString name)]

dirRename :: Text -> TypeValue
dirRename name = TypeValueObject "Rename" [("name", TypeValueString name)]

instance Meta (TypeDefinition c CONST) where
  getDirs TypeDefinition {typeContent, typeDirectives, typeDescription} = do
    contentD <- getDirs typeContent
    typeD <- traverse transform (toList typeDirectives)
    pure (contentD <> typeD <> map TypeDirectiveUsage (descDirective typeDescription))
    where
      transform v = TypeDirectiveUsage <$> directiveTypeValue v

instance Meta (TypeContent a c CONST) where
  getDirs DataObject {objectFields} = getDirs objectFields
  getDirs DataInputObject {inputObjectFields} = getDirs inputObjectFields
  getDirs DataInterface {interfaceFields} = getDirs interfaceFields
  getDirs DataEnum {enumMembers} = concat <$> traverse getDirs enumMembers
  getDirs _ = pure []

instance Meta (DataEnumValue CONST) where
  getDirs DataEnumValue {enumName, enumDirectives, enumDescription} = do
    dirs <- traverse directiveTypeValue (toList enumDirectives)
    name <- getFullName <$> getEnumName enumName
    pure $ map (EnumDirectiveUsage name) (dirs <> descDirective enumDescription)

instance Meta (FieldsDefinition c CONST) where
  getDirs = fmap concat . traverse getDirs . toList

instance Meta (FieldDefinition c CONST) where
  getDirs FieldDefinition {fieldName, fieldDirectives, fieldDescription} = do
    dirs <- traverse directiveTypeValue (toList fieldDirectives)
    name <- getFieldName fieldName
    pure $ map (FieldDirectiveUsage name) (dirs <> descDirective fieldDescription)

getInputFields :: TypeDefinition c s -> [FieldDefinition IN s]
getInputFields TypeDefinition {typeContent = DataInputObject {inputObjectFields}} = toList inputObjectFields
getInputFields _ = []

getDefaultValue :: FieldDefinition c s -> Maybe (Text, AST.Value s)
getDefaultValue
  FieldDefinition
    { fieldName,
      fieldContent = Just DefaultInputValue {defaultInputValue}
    } = Just (unpackName fieldName, defaultInputValue)
getDefaultValue _ = Nothing

nativeDirectives :: AST.DirectivesDefinition CONST
nativeDirectives = AST.directiveDefinitions internalSchema

getDirective :: (MonadReader (TypeContext CONST) m, MonadFail m) => FieldName -> m (DirectiveDefinition CONST)
getDirective directiveName = do
  dirs <- asks directiveDefinitions
  case find (\DirectiveDefinition {directiveDefinitionName} -> directiveDefinitionName == directiveName) dirs of
    Just dir -> pure dir
    _ -> selectOr (fail $ "unknown directive" <> show directiveName) pure directiveName nativeDirectives

directiveTypeValue :: MonadFail m => Directive CONST -> CodeGenT m TypeValue
directiveTypeValue Directive {..} = inType typeContext $ do
  dirs <- getDirective directiveName
  TypeValueObject typename <$> traverse (renderArgumentValue directiveArgs) (toList $ directiveDefinitionArgs dirs)
  where
    (typeContext, typename) = renderDirectiveTypeName directiveName

renderDirectiveTypeName :: FieldName -> (Maybe TypeName, TypeName)
renderDirectiveTypeName "deprecated" = (Nothing, "Deprecated")
renderDirectiveTypeName name = (Just (coerce name), coerce name)

renderArgumentValue ::
  (IsMap FieldName c, MonadFail m) =>
  c (Argument CONST) ->
  ArgumentDefinition s ->
  ReaderT (TypeContext CONST) m (FieldName, TypeValue)
renderArgumentValue args ArgumentDefinition {..} = do
  let dirName = AST.fieldName argument
  gqlValue <- selectOr (pure AST.Null) (pure . argumentValue) dirName args
  typeValue <- mapWrappedValue (AST.fieldType argument) gqlValue
  fName <- getFieldName dirName
  pure (fName, typeValue)

mapField :: MonadFail m => TypeName -> ObjectEntry CONST -> CodeGenT m (FieldName, TypeValue)
mapField tName ObjectEntry {..} = do
  t <- lookupFieldType tName entryName
  value <- mapWrappedValue t entryValue
  pure (entryName, value)

expected :: MonadFail m => String -> AST.Value CONST -> CodeGenT m TypeValue
expected typ value = fail ("expected " <> typ <> ", found " <> show (render value) <> "!")

mapWrappedValue :: MonadFail m => TypeRef -> AST.Value CONST -> CodeGenT m TypeValue
mapWrappedValue (TypeRef name (AST.BaseType isRequired)) value
  | isRequired = mapValue name value
  | value == AST.Null = pure (TypedValueMaybe Nothing)
  | otherwise = TypedValueMaybe . Just <$> mapValue name value
mapWrappedValue (TypeRef name (AST.TypeList elems isRequired)) d = case d of
  AST.Null | not isRequired -> pure (TypedValueMaybe Nothing)
  (AST.List xs) -> TypedValueMaybe . Just . TypeValueList <$> traverse (mapWrappedValue (TypeRef name elems)) xs
  value -> expected "list" value

mapValue :: MonadFail m => TypeName -> AST.Value CONST -> CodeGenT m TypeValue
mapValue name (AST.List xs) = TypeValueList <$> traverse (mapValue name) xs
mapValue _ (AST.Enum name) = pure $ TypeValueObject name []
mapValue name (AST.Object fields) = TypeValueObject name <$> traverse (mapField name) (toList fields)
mapValue _ (AST.Scalar x) = mapScalarValue x
mapValue t v = expected (show t) v

mapScalarValue :: MonadFail m => AST.ScalarValue -> CodeGenT m TypeValue
mapScalarValue (AST.Int x) = pure $ TypeValueNumber (fromIntegral x)
mapScalarValue (AST.Float x) = pure $ TypeValueNumber x
mapScalarValue (AST.String x) = pure $ TypeValueString x
mapScalarValue (AST.Boolean x) = pure $ TypeValueBool x
mapScalarValue (AST.Value _) = fail "JSON objects are not supported!"

-- UTILS
