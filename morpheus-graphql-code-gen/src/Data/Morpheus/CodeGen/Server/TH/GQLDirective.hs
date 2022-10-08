{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.TH.GQLDirective
  ( deriveGQLDirective,
  )
where

import Data.Morpheus.CodeGen.Server.Internal.AST
  ( ServerConstructorDefinition (..),
    ServerTypeDefinition (..),
    TypeName,
  )
import Data.Morpheus.CodeGen.Server.TH.Utils
  ( mkTypeableConstraints,
  )
import Data.Morpheus.CodeGen.TH
  ( apply,
    applyVars,
    typeInstanceDec,
  )
import Data.Morpheus.Server.Types
  ( GQLDirective (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( DirectiveLocation (..),
  )
import Language.Haskell.TH (Dec, Name, Q, Type (..), instanceD)
import Relude hiding (Type, toString)

noVars :: [Name]
noVars = []

deriveGQLDirective :: ServerTypeDefinition -> Q [Dec]
deriveGQLDirective DirectiveTypeDefinition {..} = do
  let constrains = mkTypeableConstraints noVars
  let tName = constructorName directiveConstructor
  let typeSignature = apply ''GQLDirective [applyVars tName noVars]
  let methods = defineMethods (constructorName directiveConstructor) directiveLocations
  gqlTypeDeclaration <- instanceD constrains typeSignature methods
  pure [gqlTypeDeclaration]
deriveGQLDirective _ = pure []

defineMethods :: TypeName -> [DirectiveLocation] -> [Q Dec]
defineMethods tName locations =
  let currentType = applyVars tName noVars
      inst = typeInstanceDec ''DIRECTIVE_LOCATIONS currentType (promotedList locations)
   in [pure inst]

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
