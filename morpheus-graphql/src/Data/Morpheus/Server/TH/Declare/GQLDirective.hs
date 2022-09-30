{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Declare.GQLDirective
  ( deriveGQLDirective,
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( ServerConstructorDefinition (..),
    ServerTypeDefinition (..),
    TypeName,
  )
import Data.Morpheus.CodeGen.Internal.TH
  ( ToName (toName),
    apply,
    applyVars,
    typeInstanceDec,
  )
import Data.Morpheus.Server.TH.Utils
  ( ServerDec,
    mkTypeableConstraints,
  )
import Data.Morpheus.Server.Types.Directives
  ( GQLDirective (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( DirectiveLocation (..),
  )
import Language.Haskell.TH
  ( Dec,
    Name,
    Q,
    Type (..),
    instanceD,
  )
import Relude hiding (Type, toString)

noVars :: [Name]
noVars = []

deriveGQLDirective :: ServerTypeDefinition -> ServerDec [Dec]
deriveGQLDirective DirectiveTypeDefinition {..} = do
  let constrains = mkTypeableConstraints noVars
  let tName = constructorName directiveConstructor
  let typeSignature = apply ''GQLDirective [applyVars tName noVars]
  methods <- defineMethods (constructorName directiveConstructor) directiveLocations
  gqlTypeDeclaration <- lift (instanceD constrains typeSignature methods)
  pure [gqlTypeDeclaration]
deriveGQLDirective _ = pure []

defineMethods :: TypeName -> [DirectiveLocation] -> ServerDec [Q Dec]
defineMethods tName locations = do
  let currentType = applyVars tName noVars
  let inst = typeInstanceDec ''DIRECTIVE_LOCATIONS currentType (promotedList locations)
  pure [pure inst]

locationName :: DirectiveLocation -> Name
locationName QUERY = 'QUERY
locationName MUTATION = 'MUTATION
locationName SUBSCRIPTION = 'SUBSCRIPTION
locationName FIELD = 'FIELD
locationName FRAGMENT_DEFINITION = 'FRAGMENT_DEFINITION
locationName FRAGMENT_SPREAD = 'FRAGMENT_SPREAD
locationName INLINE_FRAGMENT = 'INLINE_FRAGMENT
locationName SCHEMA = 'SCHEMA
locationName SCALAR = 'SCALAR
locationName OBJECT = 'OBJECT
locationName FIELD_DEFINITION = 'FIELD_DEFINITION
locationName ARGUMENT_DEFINITION = 'ARGUMENT_DEFINITION
locationName INTERFACE = 'INTERFACE
locationName UNION = 'UNION
locationName ENUM = 'ENUM
locationName ENUM_VALUE = 'ENUM_VALUE
locationName INPUT_OBJECT = 'INPUT_OBJECT
locationName INPUT_FIELD_DEFINITION = 'INPUT_FIELD_DEFINITION

promotedList :: [DirectiveLocation] -> Type
promotedList =
  foldr
    (AppT . AppT PromotedConsT . PromotedT . locationName)
    PromotedNilT
