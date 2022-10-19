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
    ServerDirectiveUsage (..),
    ServerMethod (..),
  )
import Data.Morpheus.CodeGen.Server.Interpreting.Directive (dirRename, getDirs, getNamespaceDirs)
import Data.Morpheus.CodeGen.Server.Interpreting.Utils (CodeGenMonad (printWarnings), CodeGenT, TypeContext (..), getEnumName, getFieldName, inType, isParamResolverType, isSubscription)
import Data.Morpheus.CodeGen.TH (ToName (..))
import Data.Morpheus.CodeGen.Utils
  ( camelCaseTypeName,
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
import qualified Data.Morpheus.Types.Internal.AST as AST
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
            currentTypeName = Just (packName $ toHaskellTypeName $ typeName typeDef),
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
      hsTypeName = packName $ toHaskellTypeName typeName
      cgTypeName = CodeGenTypeName [] ["m" | isResolverType tKind] hsTypeName
      renameDir = [TypeDirectiveUsage (dirRename originalTypeName) | originalTypeName /= hsTypeName]
      deriveGQL = do
        namespaceDirs <- getNamespaceDirs (unpackName hsTypeName)
        dirs <- getDirs typeDef
        -- TODO: here
        pure $
          gqlTypeToInstance
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
          [('defaultValues, ProxyArgument, ServerMethodDefaultValues gqlTypeDefaultValues) | not (null gqlTypeDefaultValues)]
            <> [('directives, ProxyArgument, ServerMethodDirectives gqlTypeDirectiveUses) | not (null gqlTypeDirectiveUses)]
      }

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
        gqlTypeToInstance
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
            { fieldName = "_",
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
              gqlTypeToInstance
                GQLTypeDefinition
                  { gqlTarget = cgTypeName,
                    gqlKind = Type,
                    gqlTypeDefaultValues = fromList (mapMaybe getDefaultValue argumentFields),
                    gqlTypeDirectiveUses = namespaceDirs <> dirs
                  }
            ]
genArgumentType _ = pure []

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
