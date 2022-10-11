{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import Data.Morpheus.CodeGen.TH
  ( ToName (toName),
  )
import Data.Morpheus.CodeGen.Utils
  ( camelCaseFieldName,
    camelCaseTypeName,
    toHaskellTypeName,
  )
import Data.Morpheus.Core (internalSchema, parseDefinitions, render)
import Data.Morpheus.Error (gqlWarnings, renderGQLErrors)
import Data.Morpheus.Internal.Ext (GQLResult, Result (..))
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
    GQLError,
    IN,
    OUT,
    ObjectEntry (..),
    OperationType (Subscription),
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
    lookupWith,
    packName,
    unpackName,
  )
import qualified Data.Morpheus.Types.Internal.AST as AST
import qualified Data.Morpheus.Types.Internal.AST as V
import Language.Haskell.TH
  ( Dec (..),
    Info (..),
    Q,
    TyVarBndr,
    reify,
  )
import Relude hiding (ByteString, get)

type ServerQ m = ReaderT (TypeContext CONST) m

class (Monad m, MonadFail m) => CodeGenMonad m where
  isParametrizedType :: TypeName -> m Bool
  printWarnings :: [GQLError] -> m ()

isParametrizedHaskellType :: Info -> Bool
isParametrizedHaskellType (TyConI x) = not $ null $ getTypeVariables x
isParametrizedHaskellType _ = False

#if MIN_VERSION_template_haskell(2,17,0)
getTypeVariables :: Dec -> [TyVarBndr ()]
#else
getTypeVariables :: Dec -> [TyVarBndr]
#endif
getTypeVariables (DataD _ _ args _ _ _) = args
getTypeVariables (NewtypeD _ _ args _ _ _) = args
getTypeVariables (TySynD _ args _) = args
getTypeVariables _ = []

instance CodeGenMonad Q where
  isParametrizedType name = isParametrizedHaskellType <$> reify (toName name)
  printWarnings = gqlWarnings

instance CodeGenMonad GQLResult where
  isParametrizedType _ = pure False
  printWarnings _ = pure ()

data TypeContext s = TypeContext
  { toArgsTypeName :: FieldName -> TypeName,
    typeDefinitions :: [TypeDefinition ANY s],
    directiveDefinitions :: [DirectiveDefinition s],
    currentTypeName :: Maybe TypeName,
    currentKind :: Maybe TypeKind,
    hasNamespace :: Bool
  }

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
            dropNamespace <- defineTypeOptions KindInputObject (unpackName typename)
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
                      gqlTypeDirectiveUses = [],
                      dropNamespace
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

defineTypeOptions :: MonadReader (TypeContext s) m => TypeKind -> Text -> m (Maybe (TypeKind, Text))
defineTypeOptions kind tName = do
  namespaces <- asks hasNamespace
  pure $ if namespaces then Just (kind, tName) else Nothing

inType :: MonadReader (TypeContext s) m => Maybe TypeName -> m a -> m a
inType name = local (\x -> x {currentTypeName = name, currentKind = Nothing})

mkInterfaceName :: TypeName -> TypeName
mkInterfaceName = ("Interface" <>)

mkPossibleTypesName :: TypeName -> TypeName
mkPossibleTypesName = ("PossibleTypes" <>)

genTypeDefinition ::
  CodeGenMonad m =>
  TypeDefinition ANY CONST ->
  ServerQ m [ServerDeclaration]
genTypeDefinition
  typeDef@TypeDefinition {typeName = originalTypeName, typeContent} =
    case tKind of
      KindScalar -> do
        scalarGQLType <- deriveGQL
        pure
          [ ScalarType (toHaskellTypeName typeName),
            scalarGQLType
          ]
      _ -> genTypeContent originalTypeName typeContent >>= withType
    where
      typeName = case typeContent of
        DataInterface {} -> mkInterfaceName originalTypeName
        _ -> originalTypeName
      tKind = kindOf typeDef
      cgTypeName = CodeGenTypeName [] ["m" | isResolverType tKind] (packName $ toHaskellTypeName typeName)
      deriveGQL = do
        gqlTypeDirectiveUses <- getDirs typeDef
        dropNamespace <- defineTypeOptions tKind (unpackName typeName)
        pure $
          GQLTypeInstance $
            GQLTypeDefinition
              { gqlTarget = cgTypeName,
                gqlTypeDirectiveUses,
                gqlKind = derivingKind tKind,
                gqlTypeDefaultValues =
                  fromList $
                    mapMaybe getDefaultValue $
                      getInputFields typeDef,
                dropNamespace
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

isParametrizedResolverType :: CodeGenMonad m => TypeName -> [TypeDefinition ANY s] -> m Bool
isParametrizedResolverType "__TypeKind" _ = pure False
isParametrizedResolverType "Boolean" _ = pure False
isParametrizedResolverType "String" _ = pure False
isParametrizedResolverType "Int" _ = pure False
isParametrizedResolverType "Float" _ = pure False
isParametrizedResolverType name lib = case lookupWith typeName name lib of
  Just x -> pure (isResolverType x)
  Nothing -> isParametrizedType name

isSubscription :: TypeKind -> Bool
isSubscription (KindObject (Just Subscription)) = True
isSubscription _ = False

mkObjectField ::
  CodeGenMonad m =>
  FieldDefinition OUT CONST ->
  ServerQ m CodeGenField
mkObjectField
  FieldDefinition
    { fieldName = fName,
      fieldContent,
      fieldType = TypeRef {typeConName, typeWrappers}
    } = do
    isParametrized <- lift . isParametrizedResolverType typeConName =<< asks typeDefinitions
    genName <- asks toArgsTypeName
    kind <- asks currentKind
    fieldName <- renderFieldName fName
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

genInterfaceUnion :: Monad m => TypeName -> ServerQ m [ServerDeclaration]
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
              gqlTypeDefaultValues = mempty,
              dropNamespace = Nothing
            }
      ]
      where
        possTypeName = CodeGenTypeName [] ["m"] (packName $ toHaskellTypeName tName)
    mkGuardWithPossibleType = InterfaceType . InterfaceDefinition interfaceName (mkInterfaceName interfaceName)
    tName = mkPossibleTypesName interfaceName

renderFieldName :: Monad m => FieldName -> ServerQ m FieldName
renderFieldName fieldName = do
  TypeContext {hasNamespace, currentTypeName} <- ask
  pure $
    if hasNamespace
      then maybe fieldName (`camelCaseFieldName` fieldName) currentTypeName
      else fieldName

mkConsEnum :: Monad m => TypeName -> DataEnumValue CONST -> ServerQ m CodeGenConstructor
mkConsEnum name DataEnumValue {enumName} = do
  namespace <- asks hasNamespace
  pure
    CodeGenConstructor
      { constructorName =
          if namespace
            then CodeGenTypeName [coerce name] [] enumName
            else fromTypeName enumName,
        constructorFields = []
      }

renderDataField :: Monad m => FieldDefinition c CONST -> ServerQ m CodeGenField
renderDataField FieldDefinition {fieldType = TypeRef {typeConName, typeWrappers}, fieldName = fName} = do
  fieldName <- renderFieldName fName
  let wrappers = [GQL_WRAPPER typeWrappers]
  let fieldType = packName (toHaskellTypeName typeConName)
  let fieldIsNullable = isNullable typeWrappers
  pure CodeGenField {..}

genTypeContent ::
  CodeGenMonad m =>
  TypeName ->
  TypeContent TRUE ANY CONST ->
  ServerQ m BuildPlan
genTypeContent _ DataScalar {} = pure (ConsIN [])
genTypeContent typeName (DataEnum tags) = ConsIN <$> traverse (mkConsEnum typeName) tags
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

genArgumentTypes :: MonadFail m => FieldsDefinition OUT CONST -> ServerQ m [ServerDeclaration]
genArgumentTypes = fmap concat . traverse genArgumentType . toList

genArgumentType :: MonadFail m => FieldDefinition OUT CONST -> ServerQ m [ServerDeclaration]
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
          gqlTypeDirectiveUses <- concat <$> traverse getDirs argumentFields
          dropNamespace <- defineTypeOptions KindInputObject typename
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
                    gqlTypeDirectiveUses,
                    dropNamespace
                  }
            ]
genArgumentType _ = pure []

-- mkFieldDescription :: FieldDefinition cat s -> Maybe (Text, Description)
-- mkFieldDescription FieldDefinition {..} = (unpackName fieldName,) <$> fieldDescription

---

class Meta a where
  getDirs :: MonadFail m => a -> ServerQ m [ServerDirectiveUsage]

instance (Meta a) => Meta (Maybe a) where
  getDirs (Just x) = getDirs x
  getDirs _ = pure []

descDirective :: Maybe Description -> [TypeValue]
descDirective desc = map describe (maybeToList desc)
  where
    describe x = TypeValueObject "Describe" [("text", TypeValueString x)]

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
    pure $ map (EnumDirectiveUsage enumName) (dirs <> descDirective enumDescription)

instance Meta (FieldsDefinition c CONST) where
  getDirs = fmap concat . traverse getDirs . toList

instance Meta (FieldDefinition c CONST) where
  getDirs FieldDefinition {fieldName, fieldDirectives, fieldDescription} = do
    dirs <- traverse directiveTypeValue (toList fieldDirectives)
    pure $ map (FieldDirectiveUsage fieldName) (dirs <> descDirective fieldDescription)

getInputFields :: TypeDefinition c s -> [FieldDefinition IN s]
getInputFields TypeDefinition {typeContent = DataInputObject {inputObjectFields}} = toList inputObjectFields
getInputFields _ = []

getDefaultValue :: FieldDefinition c s -> Maybe (Text, V.Value s)
getDefaultValue
  FieldDefinition
    { fieldName,
      fieldContent = Just DefaultInputValue {defaultInputValue}
    } = Just (unpackName fieldName, defaultInputValue)
getDefaultValue _ = Nothing

nativeDirectives :: V.DirectivesDefinition CONST
nativeDirectives = AST.directiveDefinitions internalSchema

getDirective :: (MonadReader (TypeContext CONST) m, MonadFail m) => FieldName -> m (DirectiveDefinition CONST)
getDirective directiveName = do
  dirs <- asks directiveDefinitions
  case find (\DirectiveDefinition {directiveDefinitionName} -> directiveDefinitionName == directiveName) dirs of
    Just dir -> pure dir
    _ -> selectOr (fail $ "unknown directive" <> show directiveName) pure directiveName nativeDirectives

directiveTypeValue :: MonadFail m => Directive CONST -> ServerQ m TypeValue
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
  fName <- renderFieldName dirName
  pure (fName, typeValue)

notFound :: MonadFail m => String -> String -> m a
notFound name at = fail $ "can't found " <> name <> "at " <> at <> "!"

lookupType :: MonadFail m => TypeName -> ServerQ m (TypeDefinition ANY CONST)
lookupType name = do
  types <- asks typeDefinitions
  case find (\t -> typeName t == name) types of
    Just x -> pure x
    Nothing -> notFound (show name) "type definitions"

lookupValueFieldType :: MonadFail m => TypeName -> FieldName -> ServerQ m TypeRef
lookupValueFieldType name fieldName = do
  TypeDefinition {typeContent} <- lookupType name
  case typeContent of
    DataInputObject fields -> do
      FieldDefinition {fieldType} <- selectOr (notFound (show fieldName) (show name)) pure fieldName fields
      pure fieldType
    _ -> notFound "input object" (show name)

mapField :: MonadFail m => TypeName -> ObjectEntry CONST -> ServerQ m (FieldName, TypeValue)
mapField tName ObjectEntry {..} = do
  t <- lookupValueFieldType tName entryName
  value <- mapWrappedValue t entryValue
  pure (entryName, value)

expected :: MonadFail m => String -> V.Value CONST -> ServerQ m TypeValue
expected typ value = fail ("expected " <> typ <> ", found " <> show (render value) <> "!")

mapWrappedValue :: MonadFail m => TypeRef -> V.Value CONST -> ServerQ m TypeValue
mapWrappedValue (TypeRef name (AST.BaseType isRequired)) value
  | isRequired = mapValue name value
  | value == V.Null = pure (TypedValueMaybe Nothing)
  | otherwise = TypedValueMaybe . Just <$> mapValue name value
mapWrappedValue (TypeRef name (AST.TypeList elems isRequired)) d = case d of
  V.Null | not isRequired -> pure (TypedValueMaybe Nothing)
  (V.List xs) -> TypedValueMaybe . Just . TypeValueList <$> traverse (mapWrappedValue (TypeRef name elems)) xs
  value -> expected "list" value

mapValue :: MonadFail m => TypeName -> V.Value CONST -> ServerQ m TypeValue
mapValue name (V.List xs) = TypeValueList <$> traverse (mapValue name) xs
mapValue _ (V.Enum name) = pure $ TypeValueObject name []
mapValue name (V.Object fields) = TypeValueObject name <$> traverse (mapField name) (toList fields)
mapValue _ (V.Scalar x) = mapScalarValue x
mapValue t v = expected (show t) v

mapScalarValue :: MonadFail m => V.ScalarValue -> ServerQ m TypeValue
mapScalarValue (V.Int x) = pure $ TypeValueNumber (fromIntegral x)
mapScalarValue (V.Float x) = pure $ TypeValueNumber x
mapScalarValue (V.String x) = pure $ TypeValueString x
mapScalarValue (V.Boolean x) = pure $ TypeValueBool x
mapScalarValue (V.Value _) = fail "JSON objects are not supported!"
