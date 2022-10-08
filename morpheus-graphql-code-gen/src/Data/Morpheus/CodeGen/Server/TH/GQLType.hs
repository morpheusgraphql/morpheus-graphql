{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.TH.GQLType
  ( deriveGQLType,
  )
where

import Data.Morpheus.CodeGen.Server.Internal.AST
  ( GQLTypeDefinition (..),
    ServerConstructorDefinition (constructorName),
    ServerDirectiveUsage (..),
    ServerTypeDefinition (..),
    unpackName,
  )
import Data.Morpheus.CodeGen.Server.TH.Utils
  ( ServerDec,
    mkTypeableConstraints,
    renderTypeVars,
  )
import Data.Morpheus.CodeGen.TH
  ( PrintExp (..),
    PrintType (printType),
    apply,
    applyVars,
    funDProxy,
    typeInstanceDec,
  )
import Data.Morpheus.Server.Types
  ( GQLType (..),
    dropNamespaceOptions,
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeKind (..),
  )
import Language.Haskell.TH
  ( Dec,
    DecQ,
    ExpQ,
    Name,
    Q,
    appE,
    instanceD,
  )
import Relude hiding (toString)

deriveGQLType :: ServerTypeDefinition -> ServerDec [Dec]
deriveGQLType ServerTypeDefinition {..} = do
  let typeVars = renderTypeVars typeParameters
  let constrains = mkTypeableConstraints typeVars
  let typeSignature = apply ''GQLType [applyVars tName typeVars]
  let methods = defineMethods tName typeVars typeGQLType
  gqlTypeDeclaration <- lift (instanceD constrains typeSignature methods)
  pure [gqlTypeDeclaration]
deriveGQLType DirectiveTypeDefinition {..} = do
  let typeVars = [] :: [Name]
  let tName = unpackName (constructorName directiveConstructor)
  let constrains = mkTypeableConstraints typeVars
  let typeSignature = apply ''GQLType [applyVars tName typeVars]
  let methods = defineMethods tName typeVars (Just directiveGQLType)
  gqlTypeDeclaration <- lift (instanceD constrains typeSignature methods)
  pure [gqlTypeDeclaration]
deriveGQLType _ = pure []

defineTypeOptions :: (TypeKind, Text) -> [DecQ]
defineTypeOptions (kind, tName) = funDProxy [('typeOptions, [|dropNamespaceOptions kind tName|])]

defineMethods ::
  Text ->
  [Name] ->
  Maybe GQLTypeDefinition ->
  [Q Dec]
defineMethods _ _ Nothing = []
defineMethods
  tName
  typeParameters
  (Just GQLTypeDefinition {..}) = do
    typeFamilies : functions <> concatMap defineTypeOptions (maybeToList dropNamespace)
    where
      functions =
        funDProxy
          [ ('defaultValues, [|gqlTypeDefaultValues|]),
            ('directives, renderDirectiveUsages gqlTypeDirectiveUses)
          ]

      typeFamilies = do
        currentType <- applyVars tName typeParameters
        typeInstanceDec ''KIND currentType <$> printType gqlKind

renderDirectiveUsages :: [ServerDirectiveUsage] -> ExpQ
renderDirectiveUsages = foldr (appE . appE [|(<>)|] . printExp) [|mempty|]
