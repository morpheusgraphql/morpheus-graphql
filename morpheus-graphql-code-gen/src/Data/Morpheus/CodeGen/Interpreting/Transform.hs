{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Interpreting.Transform
  ( parseServerTypeDefinitions,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConfig (..),
    DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
    GQLTypeDefinition (..),
    Kind (..),
    ServerConstructorDefinition (..),
    ServerDirectiveUsage (..),
    ServerFieldDefinition (..),
    ServerTypeDefinition (..),
    TypeValue (..),
  )
import Data.Morpheus.CodeGen.Internal.Name
  ( camelCaseFieldName,
    toHaskellTypeName,
  )
import Data.Morpheus.CodeGen.Internal.TH
  ( ToName (toName),
    camelCaseTypeName,
  )
import Data.Morpheus.Core (internalSchema, parseDefinitions, render)
import Data.Morpheus.Error (gqlWarnings, renderGQLErrors)
import Data.Morpheus.Internal.Ext (GQLResult, Result (..))
import Data.Morpheus.Internal.Utils (IsMap, selectOr)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    Argument (..),
    ArgumentDefinition (..),
    CONST,
    DataEnumValue (..),
    Description,
    Directive (Directive, directiveArgs, directiveName),
    DirectiveDefinition (..),
    Directives,
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
    Token,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    UnionMember (..),
    isPossibleInterfaceType,
    isResolverType,
    kindOf,
    lookupWith,
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

parseServerTypeDefinitions :: CodeGenMonad m => CodeGenConfig -> ByteString -> m [ServerTypeDefinition]
parseServerTypeDefinitions ctx txt =
  case parseDefinitions txt of
    Failure errors -> fail (renderGQLErrors errors)
    Success {result, warnings} -> printWarnings warnings >> toTHDefinitions (namespace ctx) result

toTHDefinitions ::
  CodeGenMonad m =>
  Bool ->
  [RawTypeDefinition] ->
  m [ServerTypeDefinition]
toTHDefinitions namespace defs = concat <$> traverse generateTypes defs
  where
    typeDefinitions = [td | RawTypeDefinition td <- defs]
    directiveDefinitions = [td | RawDirectiveDefinition td <- defs]
    generateTypes :: CodeGenMonad m => RawTypeDefinition -> m [ServerTypeDefinition]
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
            pure
              [ DirectiveTypeDefinition
                  { directiveConstructor = ServerConstructorDefinition (coerce directiveDefinitionName) fields,
                    directiveDerives = [SHOW, GENERIC],
                    directiveLocations = directiveDefinitionLocations,
                    directiveGQLType =
                      GQLTypeDefinition
                        { gqlKind = Type,
                          gqlTypeDescription = Nothing,
                          gqlTypeDescriptions = mempty,
                          gqlTypeDirectives = mempty,
                          gqlTypeDefaultValues = mempty,
                          gqlTypeDirectiveUses = []
                        }
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

inType :: MonadReader (TypeContext s) m => Maybe TypeName -> m a -> m a
inType name = local (\x -> x {currentTypeName = name, currentKind = Nothing})

mkInterfaceName :: TypeName -> TypeName
mkInterfaceName = ("Interface" <>)

mkPossibleTypesName :: TypeName -> TypeName
mkPossibleTypesName = ("PossibleTypes" <>)

genTypeDefinition ::
  CodeGenMonad m =>
  TypeDefinition ANY CONST ->
  ServerQ m [ServerTypeDefinition]
genTypeDefinition
  typeDef@TypeDefinition
    { typeName = originalTypeName,
      typeContent,
      typeDescription
    } = genTypeContent originalTypeName typeContent >>= withType
    where
      typeName = case typeContent of
        DataInterface {} -> mkInterfaceName originalTypeName
        _ -> originalTypeName
      tKind = kindOf typeDef
      tName = toHaskellTypeName typeName
      deriveGQL = do
        gqlTypeDescriptions <- getDesc typeDef
        gqlTypeDirectiveUses <- getDirs typeDef
        pure $
          Just
            GQLTypeDefinition
              { gqlTypeDescription = typeDescription,
                gqlTypeDescriptions,
                gqlTypeDirectives = mempty,
                gqlTypeDirectiveUses,
                gqlKind = derivingKind tKind,
                gqlTypeDefaultValues =
                  fromList $
                    mapMaybe getDefaultValue $
                      getInputFields typeDef
              }
      typeParameters
        | isResolverType tKind = ["m"]
        | otherwise = []
      derives = derivesClasses (isResolverType tKind)
      -------------------------
      withType (ConsIN tCons) = do
        typeGQLType <- deriveGQL
        pure [ServerTypeDefinition {..}]
      withType (ConsOUT others tCons) = do
        typeGQLType <- deriveGQL
        pure (ServerTypeDefinition {..} : others)

derivingKind :: TypeKind -> Kind
derivingKind KindScalar = Scalar
derivingKind _ = Type

derivesClasses :: Bool -> [DerivingClass]
derivesClasses isResolver = GENERIC : [SHOW | not isResolver]

mkObjectCons :: TypeName -> [ServerFieldDefinition] -> [ServerConstructorDefinition]
mkObjectCons name = pure . ServerConstructorDefinition name

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
  ServerQ m ServerFieldDefinition
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
      ServerFieldDefinition
        { fieldType = toHaskellTypeName typeConName,
          wrappers =
            mkFieldArguments fName genName (toArgList fieldContent)
              <> [SUBSCRIPTION | fmap isSubscription kind == Just True]
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
    ] = [TAGGED_ARG fieldName fieldType]
mkFieldArguments fName genName _ = [ARG (genName fName)]

toArgList :: Maybe (FieldContent bool cat s) -> [ArgumentDefinition s]
toArgList (Just (FieldArgs args)) = toList args
toArgList _ = []

data BuildPlan
  = ConsIN [ServerConstructorDefinition]
  | ConsOUT [ServerTypeDefinition] [ServerConstructorDefinition]

genInterfaceUnion :: Monad m => TypeName -> ServerQ m [ServerTypeDefinition]
genInterfaceUnion interfaceName =
  mkInterface . map typeName . mapMaybe (isPossibleInterfaceType interfaceName)
    <$> asks typeDefinitions
  where
    tKind = KindUnion
    mkInterface [] = []
    mkInterface [possibleTypeName] = [mkGuardWithPossibleType possibleTypeName]
    mkInterface members =
      [ mkGuardWithPossibleType tName,
        ServerTypeDefinition
          { tName = toHaskellTypeName tName,
            tCons = map (mkUnionFieldDefinition tName) members,
            tKind,
            typeParameters = ["m"],
            derives = derivesClasses True,
            typeGQLType = Nothing
          }
      ]
    mkGuardWithPossibleType = ServerInterfaceDefinition interfaceName (mkInterfaceName interfaceName)
    tName = mkPossibleTypesName interfaceName

renderFieldName :: Monad m => FieldName -> ServerQ m FieldName
renderFieldName fieldName = do
  TypeContext {hasNamespace, currentTypeName} <- ask
  pure $
    if hasNamespace
      then maybe fieldName (`camelCaseFieldName` fieldName) currentTypeName
      else fieldName

mkConsEnum :: Monad m => TypeName -> DataEnumValue CONST -> ServerQ m ServerConstructorDefinition
mkConsEnum name DataEnumValue {enumName} = do
  namespace <- asks hasNamespace
  pure
    ServerConstructorDefinition
      { constructorName =
          if namespace
            then camelCaseTypeName [name] enumName
            else enumName,
        constructorFields = []
      }

renderDataField :: Monad m => FieldDefinition c CONST -> ServerQ m ServerFieldDefinition
renderDataField FieldDefinition {fieldType = TypeRef {typeConName, typeWrappers}, fieldName = fName} = do
  fieldName <- renderFieldName fName
  let wrappers = [GQL_WRAPPER typeWrappers]
  let fieldType = toHaskellTypeName typeConName
  pure ServerFieldDefinition {..}

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

mkUnionFieldDefinition :: TypeName -> TypeName -> ServerConstructorDefinition
mkUnionFieldDefinition typeName memberName =
  ServerConstructorDefinition
    { constructorName,
      constructorFields =
        [ ServerFieldDefinition
            { fieldName = coerce ("un" <> constructorName),
              fieldType = toHaskellTypeName memberName,
              wrappers = [PARAMETRIZED]
            }
        ]
    }
  where
    constructorName = camelCaseTypeName [typeName] memberName

genArgumentTypes :: Monad m => FieldsDefinition OUT CONST -> ServerQ m [ServerTypeDefinition]
genArgumentTypes = fmap concat . traverse genArgumentType . toList

genArgumentType :: Monad m => FieldDefinition OUT CONST -> ServerQ m [ServerTypeDefinition]
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
          let tKind = KindInputObject
          pure
            [ ServerTypeDefinition
                { tName = toHaskellTypeName tName,
                  tKind,
                  tCons = mkObjectCons tName fields,
                  derives = derivesClasses False,
                  typeParameters = [],
                  typeGQLType =
                    Just
                      ( GQLTypeDefinition
                          { gqlKind = Type,
                            gqlTypeDescription = Nothing,
                            gqlTypeDescriptions = fromList (mapMaybe mkFieldDescription argumentFields),
                            gqlTypeDirectives = fromList (mkFieldDirective <$> argumentFields),
                            gqlTypeDefaultValues = fromList (mapMaybe getDefaultValue argumentFields),
                            gqlTypeDirectiveUses = []
                          }
                      )
                }
            ]
genArgumentType _ = pure []

mkFieldDescription :: FieldDefinition cat s -> Maybe (Text, Description)
mkFieldDescription FieldDefinition {..} = (unpackName fieldName,) <$> fieldDescription

mkFieldDirective :: FieldDefinition cat s -> (Text, Directives s)
mkFieldDirective FieldDefinition {..} = (unpackName fieldName, fieldDirectives)

---

getDesc :: MonadFail m => TypeDefinition c CONST -> ServerQ m (Map Token Description)
getDesc = fmap fromList . get

getDirs :: MonadFail m => TypeDefinition c CONST -> ServerQ m [ServerDirectiveUsage]
getDirs x = do
  contentD <- map snd <$> get x
  typeD <- traverse transform (toList $ AST.typeDirectives x)
  pure (contentD <> typeD)
  where
    transform v = TypeDirectiveUsage <$> directiveTypeValue v

class Meta a v where
  get :: MonadFail m => a -> ServerQ m [(Token, v)]

instance (Meta a v) => Meta (Maybe a) v where
  get (Just x) = get x
  get _ = pure []

instance
  ( Meta (FieldsDefinition IN s) v,
    Meta (FieldsDefinition OUT s) v,
    Meta (DataEnumValue s) v
  ) =>
  Meta (TypeDefinition c s) v
  where
  get TypeDefinition {typeContent} = get typeContent

instance
  ( Meta (FieldsDefinition IN s) v,
    Meta (FieldsDefinition OUT s) v,
    Meta (DataEnumValue s) v
  ) =>
  Meta (TypeContent a c s) v
  where
  get DataObject {objectFields} = get objectFields
  get DataInputObject {inputObjectFields} = get inputObjectFields
  get DataInterface {interfaceFields} = get interfaceFields
  get DataEnum {enumMembers} = concat <$> traverse get enumMembers
  get _ = pure []

instance Meta (DataEnumValue CONST) Description where
  get DataEnumValue {enumName, enumDescription = Just x} = pure [(unpackName enumName, x)]
  get _ = pure []

instance Meta (DataEnumValue CONST) ServerDirectiveUsage where
  get DataEnumValue {enumName, enumDirectives}
    | null enumDirectives = pure []
    | otherwise = traverse transform (toList enumDirectives)
    where
      transform x = (unpackName enumName,) . EnumDirectiveUsage enumName <$> directiveTypeValue x

instance
  Meta (FieldDefinition c CONST) v =>
  Meta (FieldsDefinition c CONST) v
  where
  get = fmap concat . traverse get . toList

instance Meta (FieldDefinition c CONST) Description where
  get FieldDefinition {fieldName, fieldDescription = Just x} = pure [(unpackName fieldName, x)]
  get _ = pure []

instance Meta (FieldDefinition c CONST) ServerDirectiveUsage where
  get FieldDefinition {fieldName, fieldDirectives}
    | null fieldDirectives = pure []
    | otherwise = traverse transform (toList fieldDirectives)
    where
      transform x = (unpackName fieldName,) . FieldDirectiveUsage fieldName <$> directiveTypeValue x

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
    _ -> selectOr (fail $ "TODO: fix me: unknown directive" <> show directiveName) pure directiveName nativeDirectives

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
  let TypeRef name wrappers = AST.fieldType argument
  typeValue <- mapWrappedValue name wrappers gqlValue
  fName <- renderFieldName dirName
  pure (fName, typeValue)

mapWrappedValue :: MonadFail m => TypeName -> AST.TypeWrapper -> V.Value CONST -> ServerQ m TypeValue
mapWrappedValue name (AST.BaseType False) value
  | value == V.Null = pure (TypedValueMaybe Nothing)
  | otherwise = TypedValueMaybe . Just <$> mapValue name value
mapWrappedValue name (AST.TypeList elems False) d = case d of
  V.Null -> pure (TypedValueMaybe Nothing)
  (V.List xs) -> TypedValueMaybe . Just . TypeValueList <$> traverse (mapWrappedValue name elems) xs
  _ -> fail "TODO: fix me"
mapWrappedValue _ _ _ = fail "TODO: fix me"

mapField :: MonadFail m => TypeName -> ObjectEntry CONST -> ServerQ m (FieldName, TypeValue)
mapField name ObjectEntry {..} = do
  value <- mapValue name entryValue
  pure (entryName, value)

mapValue :: MonadFail m => TypeName -> V.Value CONST -> ServerQ m TypeValue
mapValue name (V.List xs) = TypeValueList <$> traverse (mapValue name) xs
mapValue _ (V.Enum name) = pure $ TypeValueObject name []
mapValue name (V.Object fields) = TypeValueObject name <$> traverse (mapField name) (toList fields)
mapValue _ (V.Scalar x) = mapScalarValue x
mapValue t v = fail $ "unexpected value " <> show (render v) <> ", when expecting " <> show (render t) <> "!"

mapScalarValue :: MonadFail m => V.ScalarValue -> ServerQ m TypeValue
mapScalarValue (V.Int x) = pure $ TypeValueNumber (fromIntegral x)
mapScalarValue (V.Float x) = pure $ TypeValueNumber x
mapScalarValue (V.String x) = pure $ TypeValueString x
mapScalarValue (V.Boolean x) = pure $ TypeValueBool x
mapScalarValue (V.Value _) = fail "JSON objects are not supported!"
