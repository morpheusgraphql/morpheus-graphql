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
import Data.Morpheus.Server.Types.Internal
  ( dropNamespaceOptions,
  )
import Data.Morpheus.Types
  ( GQLType (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    TypeKind (..),
  )
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
    listE,
    litE,
    recConE,
    stringL,
  )
import Relude hiding (toString)

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
            ('defaultValues, [|gqlTypeDefaultValues|]),
            ('directives, renderDirectiveUsages gqlTypeDirectiveUses)
          ]

      typeFamilies = do
        currentType <- applyVars tName typeParameters
        pure $ typeInstanceDec ''KIND currentType (ConT (kindName gqlKind))

kindName :: Kind -> Name
kindName Scalar = ''SCALAR
kindName Type = ''TYPE

renderDirectiveUsages :: [ServerDirectiveUsage] -> ExpQ
renderDirectiveUsages =
  foldr
    (appE . appE [|(<>)|] . renderDirectiveUsage)
    [|mempty|]

renderDirectiveUsage :: ServerDirectiveUsage -> ExpQ
renderDirectiveUsage (TypeDirectiveUsage x) = [|typeDirective $(renderValue x)|]
renderDirectiveUsage (FieldDirectiveUsage field x) = [|fieldDirective field $(renderValue x)|]
renderDirectiveUsage (EnumDirectiveUsage enum x) = [|enumDirective enum $(renderValue x)|]

renderField :: (FieldName, TypeValue) -> Q FieldExp
renderField (fName, fValue) = do
  v <- renderValue fValue
  pure (toName fName, v)

renderValue :: TypeValue -> ExpQ
renderValue (TypeValueObject name xs) = recConE (toName name) (map renderField xs)
renderValue (TypeValueNumber x) = [|x|]
renderValue (TypeValueString x) = litE (stringL (T.unpack x))
renderValue (TypeValueBool _) = [|x|]
renderValue (TypedValueMaybe (Just x)) = appE (conE 'Just) (renderValue x)
renderValue (TypedValueMaybe Nothing) = conE 'Nothing
renderValue (TypeValueList xs) = listE $ map renderValue xs
