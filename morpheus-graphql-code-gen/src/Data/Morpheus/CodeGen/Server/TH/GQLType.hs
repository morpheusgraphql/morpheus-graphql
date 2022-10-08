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
  ( CodeGenConfig (..),
    GQLTypeDefinition (..),
    Kind (..),
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
    apply,
    applyVars,
    funDSimple,
    typeInstanceDec,
    _',
  )
import Data.Morpheus.Server.Types
  ( GQLType (..),
    SCALAR,
    TYPE,
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
    Type (ConT),
    appE,
    instanceD,
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
        { gqlTypeDefaultValues,
          gqlTypeDirectiveUses,
          gqlKind
        }
    ) = do
    options <- defineTypeOptions tName kind
    pure (typeFamilies : functions <> options)
    where
      functions =
        funDProxy
          [ ('defaultValues, [|gqlTypeDefaultValues|]),
            ('directives, renderDirectiveUsages gqlTypeDirectiveUses)
          ]

      typeFamilies = do
        currentType <- applyVars tName typeParameters
        pure $ typeInstanceDec ''KIND currentType (ConT (kindName gqlKind))

kindName :: Kind -> Name
kindName Scalar = ''SCALAR
kindName Type = ''TYPE

renderDirectiveUsages :: [ServerDirectiveUsage] -> ExpQ
renderDirectiveUsages = foldr (appE . appE [|(<>)|] . printExp) [|mempty|]

funDProxy :: [(Name, ExpQ)] -> [DecQ]
funDProxy = map fun
  where
    fun (name, body) = funDSimple name [_'] body
