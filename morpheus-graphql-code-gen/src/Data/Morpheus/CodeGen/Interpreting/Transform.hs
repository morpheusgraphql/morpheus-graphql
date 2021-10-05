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
{-# LANGUAGE CPP #-}

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
    ServerFieldDefinition (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.Core
  ( parseTypeDefinitions,
  )
import Data.Morpheus.Error (gqlWarnings, renderGQLErrors)
import Data.Morpheus.Internal.Ext (GQLResult, Result (..))
import Data.Morpheus.Internal.TH (ToName (toName), camelCaseFieldName)
import Data.Morpheus.Internal.Utils
  ( camelCaseTypeName,
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentDefinition (..),
    CONST,
    DataEnumValue (..),
    Description,
    Directives,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    GQLError,
    IN,
    OUT,
    OperationType (Subscription),
    TRUE,
    Token,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    UnionMember (..),
    Value,
    isPossibleInterfaceType,
    isResolverType,
    kindOf,
    lookupWith,
    mkTypeRef,
    unpackName,
  )
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
    schema :: [TypeDefinition ANY s],
    currentTypeName :: TypeName,
    hasNamespace :: Bool,
    currentKind :: Maybe TypeKind
  }

parseServerTypeDefinitions :: CodeGenMonad m => CodeGenConfig -> ByteString -> m [ServerTypeDefinition]
parseServerTypeDefinitions ctx txt =
  case parseTypeDefinitions txt of
    Failure errors -> fail (renderGQLErrors errors)
    Success {result = schema, warnings} -> printWarnings warnings >> toTHDefinitions (namespace ctx) schema

toTHDefinitions ::
  CodeGenMonad m =>
  Bool ->
  [TypeDefinition ANY CONST] ->
  m [ServerTypeDefinition]
toTHDefinitions namespace schema = concat <$> traverse generateTypes schema
  where
    generateTypes :: CodeGenMonad m => TypeDefinition ANY CONST -> m [ServerTypeDefinition]
    generateTypes typeDef =
      runReaderT
        (genTypeDefinition typeDef)
        TypeContext
          { toArgsTypeName = mkArgsTypeName namespace (typeName typeDef),
            schema,
            currentTypeName = typeName typeDef,
            currentKind = Just (kindOf typeDef),
            hasNamespace = namespace
          }

inType :: MonadReader (TypeContext s) m => TypeName -> m a -> m a
inType currentTypeName = local (\x -> x {currentTypeName, currentKind = Nothing})

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
    } = withType <$> genTypeContent originalTypeName typeContent
    where
      typeName = case typeContent of
        DataInterface {} -> mkInterfaceName originalTypeName
        _ -> originalTypeName
      tKind = kindOf typeDef
      tName = typeName
      gql =
        Just
          GQLTypeDefinition
            { gqlTypeDescription = typeDescription,
              gqlTypeDescriptions = getDesc typeDef,
              gqlTypeDirectives = getDirs typeDef,
              gqlKind = derivingKind tKind,
              gqlTypeDefaultValues =
                fromList
                  $ mapMaybe getDefaultValue
                  $ getInputFields typeDef
            }
      typeParameters
        | isResolverType tKind = ["m"]
        | otherwise = []
      derives = derivesClasses (isResolverType tKind)
      -------------------------
      withType (ConsIN tCons) = [ServerTypeDefinition {..}]
      withType (ConsOUT others tCons) = ServerTypeDefinition {..} : others

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
      fieldType
    } = do
    isParametrized <- lift . isParametrizedResolverType (typeConName fieldType) =<< asks schema
    genName <- asks toArgsTypeName
    kind <- asks currentKind
    fieldName <- genFieldName fName
    pure
      ServerFieldDefinition
        { wrappers =
            mkFieldArguments fName genName (toArgList fieldContent)
              <> [SUBSCRIPTION | fmap isSubscription kind == Just True]
              <> [MONAD],
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
    <$> asks schema
  where
    tKind = KindUnion
    mkInterface [] = []
    mkInterface [possibleTypeName] = [mkGuardWithPossibleType possibleTypeName]
    mkInterface members =
      [ mkGuardWithPossibleType tName,
        ServerTypeDefinition
          { tName,
            tCons = map (mkUnionFieldDefinition tName) members,
            tKind,
            typeParameters = ["m"],
            derives = derivesClasses True,
            gql = Nothing
          }
      ]
    mkGuardWithPossibleType = ServerInterfaceDefinition interfaceName (mkInterfaceName interfaceName)
    tName = mkPossibleTypesName interfaceName

genFieldName :: Monad m => FieldName -> ServerQ m FieldName
genFieldName fieldName = do
  TypeContext {hasNamespace, currentTypeName} <- ask
  pure $
    if hasNamespace
      then camelCaseFieldName currentTypeName fieldName
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

toNonResolverServerField :: Monad m => FieldDefinition c CONST -> ServerQ m ServerFieldDefinition
toNonResolverServerField FieldDefinition {fieldType, fieldName = fName} = do
  fieldName <- genFieldName fName
  pure $
    ServerFieldDefinition
      { isParametrized = False,
        fieldType,
        fieldName,
        wrappers = []
      }

genTypeContent ::
  CodeGenMonad m =>
  TypeName ->
  TypeContent TRUE ANY CONST ->
  ServerQ m BuildPlan
genTypeContent _ DataScalar {} = pure (ConsIN [])
genTypeContent typeName (DataEnum tags) = ConsIN <$> traverse (mkConsEnum typeName) tags
genTypeContent typeName (DataInputObject fields) =
  ConsIN . mkObjectCons typeName <$> traverse toNonResolverServerField (toList fields)
genTypeContent _ DataInputUnion {} = fail "Input Unions not Supported"
genTypeContent typeName DataInterface {interfaceFields} =
  ConsOUT
    <$> ((<>) <$> genArgumentTypes interfaceFields <*> genInterfaceUnion typeName)
    <*> ( do
            let interfaceName = mkInterfaceName typeName
            inType
              interfaceName
              ( mkObjectCons interfaceName
                  <$> traverse mkObjectField (toList interfaceFields)
              )
        )
genTypeContent typeName DataObject {objectFields} =
  ConsOUT <$> genArgumentTypes objectFields
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
            { isParametrized = True,
              fieldName = coerce ("un" <> constructorName),
              fieldType = mkTypeRef memberName,
              wrappers = []
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
      inType tName $ do
        let argumentFields = argument <$> toList arguments
        fields <- traverse toNonResolverServerField argumentFields
        let tKind = KindInputObject
        pure
          [ ServerTypeDefinition
              { tName,
                tKind,
                tCons = mkObjectCons tName fields,
                derives = derivesClasses False,
                typeParameters = [],
                gql =
                  Just
                    ( GQLTypeDefinition
                        { gqlKind = Type,
                          gqlTypeDescription = Nothing,
                          gqlTypeDescriptions = fromList (mapMaybe mkFieldDescription argumentFields),
                          gqlTypeDirectives = fromList (mkFieldDirective <$> argumentFields),
                          gqlTypeDefaultValues = fromList (mapMaybe getDefaultValue argumentFields)
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

getDesc :: TypeDefinition c s -> Map Token Description
getDesc = fromList . get

getDirs :: TypeDefinition c s -> Map Token (Directives s)
getDirs = fromList . get

class Meta a v where
  get :: a -> [(Token, v)]

instance (Meta a v) => Meta (Maybe a) v where
  get (Just x) = get x
  get _ = []

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
  get DataEnum {enumMembers} = concatMap get enumMembers
  get _ = []

instance Meta (DataEnumValue s) Description where
  get DataEnumValue {enumName, enumDescription = Just x} = [(unpackName enumName, x)]
  get _ = []

instance Meta (DataEnumValue s) (Directives s) where
  get DataEnumValue {enumName, enumDirectives}
    | null enumDirectives = []
    | otherwise = [(unpackName enumName, enumDirectives)]

instance
  Meta (FieldDefinition c s) v =>
  Meta (FieldsDefinition c s) v
  where
  get = concatMap get . toList

instance Meta (FieldDefinition c s) Description where
  get FieldDefinition {fieldName, fieldDescription = Just x} = [(unpackName fieldName, x)]
  get _ = []

instance Meta (FieldDefinition c s) (Directives s) where
  get FieldDefinition {fieldName, fieldDirectives}
    | null fieldDirectives = []
    | otherwise = [(unpackName fieldName, fieldDirectives)]

getInputFields :: TypeDefinition c s -> [FieldDefinition IN s]
getInputFields TypeDefinition {typeContent = DataInputObject {inputObjectFields}} = toList inputObjectFields
getInputFields _ = []

getDefaultValue :: FieldDefinition c s -> Maybe (Text, Value s)
getDefaultValue
  FieldDefinition
    { fieldName,
      fieldContent = Just DefaultInputValue {defaultInputValue}
    } = Just (unpackName fieldName, defaultInputValue)
getDefaultValue _ = Nothing
