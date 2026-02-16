{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.Interpreting.Transform
  ( parseServerTypeDefinitions,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.CodeGen.Internal.AST
  ( AssociatedType (..),
    CodeGenConstructor (..),
    CodeGenField (..),
    CodeGenType (..),
    CodeGenTypeName (CodeGenTypeName, typeParameters),
    MethodArgument (..),
    TypeClassInstance (..),
    fromTypeName,
    getFullName,
  )
import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
    DerivingClass (..),
    FIELD_TYPE_WRAPPER (..),
    GQLTypeDefinition (..),
    InterfaceDefinition (..),
    Kind (..),
    ServerDeclaration (..),
    ServerMethod (..),
  )
import Data.Morpheus.CodeGen.Server.Interpreting.Directive
  ( getDefaultValueDir,
    getDirectives,
    getNamespaceDirs,
    getRenameDir,
  )
import Data.Morpheus.CodeGen.Server.Interpreting.Utils
  ( CodeGenM,
    CodeGenMonad (printWarnings),
    ServerCodeGenContext (..),
    checkTypeExistence,
    getEnumName,
    getFieldName,
    getFieldTypeName,
    inType,
    isParamResolverType,
    isSubscription,
  )
import Data.Morpheus.CodeGen.TH (ToName (..))
import Data.Morpheus.CodeGen.Utils
  ( Flag (..),
    Flags,
    camelCaseTypeName,
    langExtension,
    runCodeGenT,
    toHaskellTypeName,
  )
import Data.Morpheus.Core (parseDefinitions)
import Data.Morpheus.Error (renderGQLErrors)
import Data.Morpheus.Internal.Ext (Result (..))
import Data.Morpheus.Server.Types (Arg, DIRECTIVE_LOCATIONS, GQLDirective, GQLType (..), SubscriptionField)
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ArgumentDefinition (..),
    CONST,
    DataEnumValue (..),
    DirectiveDefinition (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    IN,
    OUT,
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
import Relude hiding (ByteString, get)

parseServerTypeDefinitions :: (CodeGenMonad m) => CodeGenConfig -> ByteString -> m ([ServerDeclaration], Flags)
parseServerTypeDefinitions ctx txt =
  case parseDefinitions txt of
    Failure errors -> fail (renderGQLErrors errors)
    Success {result, warnings} -> printWarnings warnings >> toTHDefinitions (namespace ctx) result

getExternals :: [ServerDeclaration] -> Flags
getExternals xs =
  [FlagExternal scalarTypeName | ScalarType {scalarTypeName} <- xs]
    <> [FlagExternal (unpackName $ getFullName $ typeClassTarget v) | GQLTypeInstance Scalar v <- xs]

toTHDefinitions ::
  (CodeGenMonad m) =>
  Bool ->
  [RawTypeDefinition] ->
  m ([ServerDeclaration], Flags)
toTHDefinitions namespace defs = do
  (types, flags) <- bimap concat concat . unzip <$> traverse generateTypes defs
  pure (types, flags <> getExternals types)
  where
    typeDefinitions = [td | RawTypeDefinition td <- defs]
    directiveDefinitions = [td | RawDirectiveDefinition td <- defs]
    generateTypes :: (CodeGenMonad m) => RawTypeDefinition -> m ([ServerDeclaration], Flags)
    generateTypes (RawTypeDefinition typeDef) =
      runCodeGenT
        (genTypeDefinition typeDef)
        ServerCodeGenContext
          { toArgsTypeName = mkArgsTypeName namespace (typeName typeDef),
            typeDefinitions,
            directiveDefinitions,
            currentTypeName = Just (packName $ toHaskellTypeName $ typeName typeDef),
            currentKind = Just (kindOf typeDef),
            hasNamespace = namespace
          }
    generateTypes (RawDirectiveDefinition dirDef) =
      runCodeGenT
        (genDirectiveDefinition dirDef)
        ServerCodeGenContext
          { toArgsTypeName = coerce,
            typeDefinitions,
            currentTypeName = Just (coerce directiveDefinitionName dirDef),
            directiveDefinitions,
            currentKind = Nothing,
            hasNamespace = namespace
          }
    generateTypes _ = pure (mempty, mempty)

mkInterfaceName :: TypeName -> TypeName
mkInterfaceName = ("Interface" <>)

mkPossibleTypesName :: TypeName -> TypeName
mkPossibleTypesName = ("PossibleTypes" <>)

genDirectiveDefinition :: (CodeGenM m) => DirectiveDefinition CONST -> m [ServerDeclaration]
genDirectiveDefinition DirectiveDefinition {..} = do
  fields <- traverse (renderDataField . argument) (toList directiveDefinitionArgs)
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
        TypeClassInstance
          { typeClassName = ''GQLDirective,
            typeClassContext = [],
            typeClassTarget = cgTypeName,
            assoc = [(''DIRECTIVE_LOCATIONS, AssociatedLocations directiveDefinitionLocations)],
            typeClassMethods = []
          },
      gqlTypeToInstance
        GQLTypeDefinition
          { gqlTarget = cgTypeName,
            gqlKind = Directive,
            gqlTypeDirectiveUses = namespaceDirs
          }
    ]

genTypeDefinition ::
  (CodeGenM m) =>
  TypeDefinition ANY CONST ->
  m [ServerDeclaration]
genTypeDefinition
  typeDef@TypeDefinition {typeName = originalTypeName, typeContent} =
    case tKind of
      KIND_SCALAR -> do
        scalarGQLType <- deriveGQL
        pure [ScalarType (toHaskellTypeName typeName), scalarGQLType]
      _ -> genTypeContent originalTypeName typeContent >>= withType
    where
      typeName
        | tKind == KIND_INTERFACE = mkInterfaceName originalTypeName
        | otherwise = originalTypeName
      tKind = kindOf typeDef
      hsTypeName = packName $ toHaskellTypeName typeName
      cgTypeName = CodeGenTypeName [] ["m" | isResolverType tKind] hsTypeName
      deriveGQL = do
        defaultValueDirs <- concat <$> traverse getDefaultValueDir (getInputFields typeDef)
        namespaceDirs <- getNamespaceDirs (unpackName hsTypeName)
        dirs <- getDirectives typeDef
        renameDir <- getRenameDir originalTypeName hsTypeName
        pure
          $ gqlTypeToInstance
            GQLTypeDefinition
              { gqlTarget = cgTypeName,
                gqlTypeDirectiveUses = renameDir <> namespaceDirs <> dirs <> defaultValueDirs,
                gqlKind = derivingKind tKind
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
derivingKind KIND_SCALAR = Scalar
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
  (CodeGenM m) =>
  FieldDefinition OUT CONST ->
  m CodeGenField
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
    args <- mkFieldArguments fName genName (toArgList fieldContent)
    fieldType <- getFieldTypeName typeConName
    pure
      CodeGenField
        { fieldType,
          fieldIsNullable = isNullable typeWrappers,
          wrappers =
            args
              <> [SUBSCRIPTION ''SubscriptionField | fmap isSubscription kind == Just True]
              <> [MONAD]
              <> [GQL_WRAPPER typeWrappers]
              <> [PARAMETRIZED | isParametrized],
          ..
        }

mkFieldArguments :: (CodeGenM m) => FieldName -> (FieldName -> TypeName) -> [ArgumentDefinition s] -> m [FIELD_TYPE_WRAPPER]
mkFieldArguments _ _ [] = pure []
mkFieldArguments
  _
  _
  [ ArgumentDefinition FieldDefinition {fieldName, fieldType}
    ] =
    checkTypeExistence (typeConName fieldType)
      >> langExtension "DataKinds"
      $> [TAGGED_ARG ''Arg fieldName fieldType]
mkFieldArguments fName genName _ = pure [ARG (genName fName)]

toArgList :: Maybe (FieldContent bool cat s) -> [ArgumentDefinition s]
toArgList (Just (FieldArgs args)) = toList args
toArgList _ = []

data BuildPlan
  = ConsIN [CodeGenConstructor]
  | ConsOUT [ServerDeclaration] [CodeGenConstructor]

gqlTypeToInstance :: GQLTypeDefinition -> ServerDeclaration
gqlTypeToInstance GQLTypeDefinition {..} =
  GQLTypeInstance
    gqlKind
    TypeClassInstance
      { typeClassName = ''GQLType,
        typeClassContext = map ((''Typeable,) . toName) (typeParameters gqlTarget),
        typeClassTarget = gqlTarget,
        assoc = [(''KIND, AssociatedTypeName (toName gqlKind))],
        typeClassMethods =
          [ ('directives, ProxyArgument, ServerMethodDirectives gqlTypeDirectiveUses)
          | not (null gqlTypeDirectiveUses)
          ]
      }

genInterfaceUnion :: (CodeGenM m) => TypeName -> m [ServerDeclaration]
genInterfaceUnion interfaceName =
  asks typeDefinitions >>= mkInterface . map typeName . mapMaybe (isPossibleInterfaceType interfaceName)
  where
    mkInterface [] = pure []
    mkInterface [possibleTypeName] = pure [mkGuardWithPossibleType possibleTypeName]
    mkInterface members = do
      cgConstructors <- traverse (mkUnionFieldDefinition tName) members
      pure
        [ mkGuardWithPossibleType tName,
          DataType
            CodeGenType
              { cgTypeName = possTypeName,
                cgConstructors,
                cgDerivations = derivesClasses True
              },
          gqlTypeToInstance
            GQLTypeDefinition
              { gqlTarget = possTypeName,
                gqlKind = Type,
                gqlTypeDirectiveUses = empty
              }
        ]
      where
        possTypeName = CodeGenTypeName [] ["m"] (packName $ toHaskellTypeName tName)
    mkGuardWithPossibleType = InterfaceType . InterfaceDefinition interfaceName (mkInterfaceName interfaceName)
    tName = mkPossibleTypesName interfaceName

mkConsEnum :: (CodeGenM m) => DataEnumValue CONST -> m CodeGenConstructor
mkConsEnum DataEnumValue {enumName} = do
  constructorName <- getEnumName enumName
  pure CodeGenConstructor {constructorName, constructorFields = []}

renderDataField :: (CodeGenM m) => FieldDefinition c CONST -> m CodeGenField
renderDataField FieldDefinition {fieldType = TypeRef {typeConName, typeWrappers}, fieldName = fName} = do
  fieldName <- getFieldName fName
  let wrappers = [GQL_WRAPPER typeWrappers]
  fieldType <- getFieldTypeName typeConName
  let fieldIsNullable = isNullable typeWrappers
  pure CodeGenField {..}

genTypeContent :: (CodeGenM m) => TypeName -> TypeContent TRUE ANY CONST -> m BuildPlan
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
genTypeContent typeName (DataUnion members) = do
  ConsOUT [] <$> traverse unionCon (toList members)
  where
    unionCon UnionMember {memberName} = mkUnionFieldDefinition typeName memberName

mkUnionFieldDefinition :: (CodeGenM m) => TypeName -> TypeName -> m CodeGenConstructor
mkUnionFieldDefinition typeName memberName = do
  fieldType <- getFieldTypeName memberName
  pure
    $ CodeGenConstructor
      { constructorName,
        constructorFields =
          [ CodeGenField
              { fieldName = "_",
                fieldType,
                wrappers = [MONAD, PARAMETRIZED],
                fieldIsNullable = False
              }
          ]
      }
  where
    constructorName = CodeGenTypeName [coerce typeName] [] memberName

genArgumentTypes :: (CodeGenM m) => FieldsDefinition OUT CONST -> m [ServerDeclaration]
genArgumentTypes = fmap concat . traverse genArgumentType . toList

genArgumentType :: (CodeGenM m) => FieldDefinition OUT CONST -> m [ServerDeclaration]
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
          dirs <- concat <$> traverse getDirectives argumentFields
          let cgTypeName = fromTypeName (packName typename)
          defaultValueDirs <- concat <$> traverse getDefaultValueDir argumentFields
          pure
            [ DataType
                CodeGenType
                  { cgTypeName,
                    cgConstructors = mkObjectCons tName fields,
                    cgDerivations = derivesClasses False
                  },
              gqlTypeToInstance
                GQLTypeDefinition
                  { gqlTarget = cgTypeName,
                    gqlKind = Type,
                    gqlTypeDirectiveUses = namespaceDirs <> dirs <> defaultValueDirs
                  }
            ]
genArgumentType _ = pure []

getInputFields :: TypeDefinition c s -> [FieldDefinition IN s]
getInputFields TypeDefinition {typeContent = DataInputObject {inputObjectFields}} = toList inputObjectFields
getInputFields _ = []
