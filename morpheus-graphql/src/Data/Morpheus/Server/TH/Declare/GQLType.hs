{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Declare.GQLType
  ( deriveGQLType,
  )
where

import Data.Char (toLower)
import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConfig (..),
    GQLTypeDefinition (..),
    Kind (..),
    ServerConstructorDefinition (constructorName),
    ServerDirectiveUsage (..),
    ServerTypeDefinition (..),
    TypeValue (..),
    unpackName,
  )
import Data.Morpheus.CodeGen.Internal.TH
  ( ToName (..),
    apply,
    applyVars,
    typeInstanceDec,
  )
import Data.Morpheus.Kind
  ( SCALAR,
    TYPE,
  )
import Data.Morpheus.Server.TH.Utils
  ( ServerDec,
    funDProxy,
    mkTypeableConstraints,
    renderTypeVars,
  )
import Data.Morpheus.Server.Types.GQLType
  ( enumDirective,
    fieldDirective,
    typeDirective,
  )
import Data.Morpheus.Types
  ( GQLType (..),
    GQLTypeOptions (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( Directive (directiveName),
    FieldName,
    TypeKind (..),
  )
import Data.Text (unpack)
import qualified Data.Text as T
import Language.Haskell.TH
  ( Dec,
    DecQ,
    ExpQ,
    FieldExp,
    Name,
    Q,
    Type (ConT),
    appE,
    conE,
    instanceD,
    litE,
    recConE,
    stringL,
  )
import Relude hiding (toString)

dropPrefix :: Text -> String -> String
dropPrefix name = drop (T.length name)

stripConstructorNamespace :: Text -> String -> String
stripConstructorNamespace = dropPrefix

stripFieldNamespace :: Text -> String -> String
stripFieldNamespace prefix = __uncapitalize . dropPrefix prefix
  where
    __uncapitalize [] = []
    __uncapitalize (x : xs) = toLower x : xs

dropNamespaceOptions :: TypeKind -> Text -> GQLTypeOptions -> GQLTypeOptions
dropNamespaceOptions KindInterface tName opt =
  opt
    { typeNameModifier = const (stripConstructorNamespace "Interface"),
      fieldLabelModifier = stripFieldNamespace tName
    }
dropNamespaceOptions KindEnum tName opt = opt {constructorTagModifier = stripConstructorNamespace tName}
dropNamespaceOptions _ tName opt = opt {fieldLabelModifier = stripFieldNamespace tName}

deriveGQLType :: ServerTypeDefinition -> ServerDec [Dec]
deriveGQLType
  ServerTypeDefinition
    { tName,
      tKind,
      typeParameters,
      typeGQLType
    } = do
    let typeVars = renderTypeVars typeParameters
    let constrains = mkTypeableConstraints typeVars
    let typeSignature = apply ''GQLType [applyVars tName typeVars]
    methods <- defineMethods tName tKind typeVars typeGQLType
    gqlTypeDeclaration <- lift (instanceD constrains typeSignature methods)
    pure [gqlTypeDeclaration]
deriveGQLType DirectiveTypeDefinition {..} = do
  let typeVars = [] :: [Name]
  let tName = unpackName (constructorName directiveConstructor)
  let constrains = mkTypeableConstraints typeVars
  let typeSignature = apply ''GQLType [applyVars tName typeVars]
  methods <- defineMethods tName KindInputObject typeVars (Just directiveGQLType)
  gqlTypeDeclaration <- lift (instanceD constrains typeSignature methods)
  pure [gqlTypeDeclaration]
deriveGQLType _ = pure []

defineTypeOptions :: Text -> TypeKind -> ServerDec [DecQ]
defineTypeOptions tName kind = do
  CodeGenConfig {namespace} <- ask
  pure $ funDProxy [('typeOptions, [|dropNamespaceOptions kind tName|]) | namespace]

defineMethods ::
  Text ->
  TypeKind ->
  [Name] ->
  Maybe GQLTypeDefinition ->
  ServerDec [Q Dec]
defineMethods tName kind _ Nothing = defineTypeOptions tName kind
defineMethods
  tName
  kind
  typeParameters
  ( Just
      GQLTypeDefinition
        { gqlTypeDescription,
          gqlTypeDescriptions,
          gqlTypeDirectives,
          gqlTypeDefaultValues,
          gqlTypeDirectiveUses,
          gqlKind
        }
    ) = do
    options <- defineTypeOptions tName kind
    pure (typeFamilies : functions <> options)
    where
      functions =
        funDProxy
          [ ('description, [|gqlTypeDescription|]),
            ('getDescriptions, [|gqlTypeDescriptions|]),
            ('getDirectives, [|gqlTypeDirectives|]),
            ('defaultValues, [|gqlTypeDefaultValues|]),
            ('directives, defineDirectiveUsages gqlTypeDirectiveUses)
          ]

      typeFamilies = do
        currentType <- applyVars tName typeParameters
        pure $ typeInstanceDec ''KIND currentType (ConT (kindName gqlKind))

kindName :: Kind -> Name
kindName Scalar = ''SCALAR
kindName Type = ''TYPE

defineDirectiveUsages :: [ServerDirectiveUsage] -> ExpQ
defineDirectiveUsages =
  foldr
    (appE . appE [|(<>)|] . defineDirectiveUsage)
    [|mempty|]

defineDirectiveUsage :: ServerDirectiveUsage -> ExpQ
defineDirectiveUsage (TypeDirectiveUsage x) = [|typeDirective $(defineValue x)|]
defineDirectiveUsage (FieldDirectiveUsage field x) = [|fieldDirective field $(defineValue x)|]
defineDirectiveUsage (EnumDirectiveUsage enum x) = [|enumDirective enum $(defineValue x)|]

defineValue :: TypeValue -> ExpQ
defineValue (TypeValueObject name xs) = recConE (toName name) (map renderField xs)
  where
    renderField :: (FieldName, TypeValue) -> Q FieldExp
    renderField (fName, fValue) = do
      v <- defineValue fValue
      pure (toName fName, v)
defineValue (TypeValueNumber x) = [|x|]
defineValue (TypeValueString x) = litE (stringL (unpack x))
defineValue (TypeValueBool _) = [|x|]
defineValue (TypedValueMaybe (Just x)) = appE (conE 'Just) (defineValue x)
defineValue (TypedValueMaybe Nothing) = conE 'Nothing
-- dead codes
defineValue (TypeValueList xs) = undefined