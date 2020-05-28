{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Data.Morpheus.Types.Directive
  ( SchemaDirectiveVisitor (..),
    ResolverDirective (..),
    FieldDirective (..),
  )
where

import Data.Morpheus.Types.Internal.AST

data FieldDirective d a = FieldDirective

data DirectiveKind = SCHEMA | RESOLVER

-- visitSchema(schema: GraphQLSchema)
-- visitScalar(scalar: GraphQLScalarType)
-- visitObject(object: GraphQLObjectType)
-- visitFieldDefinition(field: GraphQLField<any, any>)
-- visitArgumentDefinition(argument: GraphQLArgument)
-- visitInterface(iface: GraphQLInterfaceType)
-- visitUnion(union: GraphQLUnionType)
-- visitEnum(type: GraphQLEnumType)
-- visitEnumValue(value: GraphQLEnumValue)
-- visitInputObject(object: GraphQLInputObjectType)
-- visitInputFieldDefinition(field: GraphQLInputField)

class ResolverDirective dir a where
  resolverDirective :: dir -> a

class SchemaDirectiveVisitor a where
  visitFieldDefinition :: a -> FieldDefinition cat -> FieldDefinition cat
  visitFieldResolver :: a -> FieldDefinition cat -> b
