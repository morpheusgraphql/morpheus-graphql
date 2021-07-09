{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Declare.GQLType
  ( deriveGQLType,
  )
where

--
-- MORPHEUS
import Data.Morpheus.Internal.TH
  ( apply,
    applyVars,
    typeInstanceDec,
  )
import Data.Morpheus.Internal.Utils
  ( stripConstructorNamespace,
    stripFieldNamespace,
  )
import Data.Morpheus.Server.Internal.TH.Types
  ( GQLTypeDefinition (..),
    ServerDec,
    ServerDecContext (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.Internal.TH.Utils
  ( funDProxy,
    mkTypeableConstraints,
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
    GQLTypeOptions (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeKind (..),
    TypeName,
  )
import Language.Haskell.TH
import Relude

dropNamespaceOptions :: TypeKind -> TypeName -> GQLTypeOptions -> GQLTypeOptions
dropNamespaceOptions KindInterface tName opt =
  opt
    { typeNameModifier = const (stripConstructorNamespace "Interface"),
      fieldLabelModifier = stripFieldNamespace tName
    }
dropNamespaceOptions KindEnum tName opt = opt {constructorTagModifier = stripConstructorNamespace tName}
dropNamespaceOptions _ tName opt = opt {fieldLabelModifier = stripFieldNamespace tName}

deriveGQLType :: ServerTypeDefinition s -> ServerDec [Dec]
deriveGQLType ServerInterfaceDefinition {} = pure []
deriveGQLType
  ServerTypeDefinition
    { tName,
      tKind,
      typeParameters,
      gql
    } = do
    let constrains = mkTypeableConstraints typeParameters
    let typeSignature = apply ''GQLType [applyVars tName typeParameters]
    methods <- defineMethods tName tKind typeParameters gql
    gqlTypeDeclaration <- lift (instanceD constrains typeSignature methods)
    pure [gqlTypeDeclaration]

defineTypeOptions :: TypeName -> TypeKind -> ServerDec [DecQ]
defineTypeOptions tName kind = do
  ServerDecContext {namespace} <- ask
  pure $ funDProxy [('typeOptions, [|dropNamespaceOptions kind tName|]) | namespace]

defineMethods :: TypeName -> TypeKind -> [Name] -> Maybe (GQLTypeDefinition s) -> ServerDec [Q Dec]
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
            ('defaultValues, [|gqlTypeDefaultValues|])
          ]
      typeFamilies = do
        currentType <- applyVars tName typeParameters
        pure $ typeInstanceDec ''KIND currentType (ConT gqlKind)
